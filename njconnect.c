/* -*- Mode: C ; c-basic-offset: 2 -*- */
/*****************************************************************************
 *
 * ncurses Jack patchbay
 *
 * Copyright (C) 2012-2013 Xj <xj@wp.pl>
 *   with lots of patches from G.raud Meyer
 *
 * based on naconnect by Nedko Arnaudov <nedko@arnaudov.name>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 2 of.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *****************************************************************************/

#include <ncurses.h>
#include <string.h>
#include <jack/jack.h>
#include <jack/jslist.h>
#include <stdbool.h>

#define APPNAME "njconnect"
#define VERSION "1.5"
#define CON_NAME_A "Audio Connections"
#define CON_NAME_M "MIDI Connections"

#define ERR_CONNECT "Connection failed"
#define ERR_DISCONNECT "Disconnection failed"
#define GRAPH_CHANGED "Graph changed"
#define DEFAULT_STATUS "->> Press SHIFT+H or ? for help << -"
#define KEY_TAB '\t'

#define WOUT_X 0
#define WOUT_Y 0
#define WOUT_W cols / 2
#define WOUT_H rows / 2

#define WIN_X cols / 2
#define WIN_Y 0
#define WIN_W cols - cols / 2
#define WIN_H rows / 2

#define WCON_X 0
#define WCON_Y rows / 2
#define WCON_W cols
#define WCON_H rows - rows / 2 - 1

#define WSTAT_X 0
#define WSTAT_Y rows - 1
#define WSTAT_W cols
#define WSTAT_H 0

#define MSG_OUT(format, arg...) printf(format "\n", ## arg)
#define ERR_OUT(format, arg...) ( endwin(), fprintf(stderr, format "\n", ## arg), refresh() )

enum WinType {
   WIN_PORTS,
   WIN_CONNECTIONS
};

struct window {
  WINDOW * window_ptr;
  JSList* list_ptr;
  bool selected;
  int height;
  int width;
  const char * name;
  unsigned short index;
  unsigned short count;
  enum WinType type;
};

struct port {
   char name[128];
   char type[32];
   int flags;
};

struct connection {
   const char* type;
   struct port* in;
   struct port* out;
};

struct graph {
   bool* want_refresh;
   const char** err_message;
};

/* Function forgotten by Jack-Devs */
JSList* jack_slist_nth(JSList* list_ptr, unsigned short n) {
   unsigned short i = 0;

   JSList* node;
   for(node=list_ptr; node ; node = jack_slist_next(node), i++ )
      if (i == n) return node;

   return NULL;
}

int jack_slist_find_pos(JSList* list_ptr, void *data) {
   unsigned short i = 0;

   JSList* node;
   for(node=list_ptr; node ; node = jack_slist_next(node), i++ )
      if (node->data == data) return i;

   return -1;
}

void window_item_next(struct window* w) { if (w->index < w->count - 1) w->index++; }
void window_item_previous(struct window* w) { if (w->index > 0) w->index--; }

void suppress_jack_log(const char* msg) {
	/* Just suppress Jack SPAM here ;-) */
}

JSList* build_ports(jack_client_t* client) {
   JSList* new = NULL;
   jack_port_t* jp;
   unsigned short i, count=0;
   struct port* p;

   const char** jports = jack_get_ports (client, NULL, NULL, 0);
   if(! jports) return NULL;

   while(jports[count]) count++;
   p = calloc(count, sizeof(struct port));

   for (i=0; jports[i]; ++i, p++) {
       jp = jack_port_by_name( client, jports[i] );

       strncpy(p->name, jports[i], sizeof(p->name));
       strncpy(p->type, jack_port_type( jp ), sizeof(p->type));
       p->flags = jack_port_flags( jp );
       new = jack_slist_append(new, p);
   }
   jack_free(jports);
   
   return new;
}

JSList*
select_ports(JSList* list_ptr, int flags, const char* type) {
   JSList* new = NULL;
   JSList* node;
   struct port* p;
   for ( node=list_ptr; node; node=jack_slist_next(node) ) {
       p = node->data;
       if ( (p->flags & flags) && strcmp(p->type, type) == 0 )
          new = jack_slist_append(new, p);
   }

   return new;
}

struct port*
get_port_by_name(JSList* list_ptr, const char* name) {
   struct port* p;
   JSList* node;

   for ( node=list_ptr; node; node=jack_slist_next(node) ) {
       p = node->data;
       if (strcmp(p->name, name) == 0) return p;
   }
   return NULL;
}

JSList*
build_connections(jack_client_t* client, JSList* list_ptr, const char* type) {
   JSList* new = NULL;

   JSList* node;
   for ( node=list_ptr; node; node=jack_slist_next(node) ) {
       // For all Input ports
       struct port *inp = node->data;
       if(! (inp->flags & JackPortIsInput)) continue;
       if( strcmp(inp->type, type) != 0 ) continue;

       const char** connections = jack_port_get_all_connections (
           client, jack_port_by_name(client, inp->name) );
       if (!connections) continue;

       unsigned short i;
       for (i=0; connections[i]; i++) {
           struct port *outp = get_port_by_name(list_ptr, connections[i]);
           if(!outp) continue; // WTF can't find OutPort in our list ?

           struct connection* c = malloc(sizeof(struct connection));
           c->type = type;
           c->in = inp;
           c->out = outp;
           new = jack_slist_append(new, c);
       }
       jack_free(connections);
   }

   return new;
}

void draw_border(struct window * window_ptr) {
  int col = (window_ptr->width - strlen(window_ptr->name) - 4)/2;
  if (col < 0) col = 0;

  /* 0, 0 gives default characters for the vertical and horizontal lines */
  box(window_ptr->window_ptr, 0, 0);

  if (window_ptr->selected) {
     wattron(window_ptr->window_ptr, WA_BOLD|COLOR_PAIR(4));
     mvwprintw( window_ptr->window_ptr, 0, col, "=[%s]=", window_ptr->name);
     wattroff(window_ptr->window_ptr, WA_BOLD|COLOR_PAIR(4));
  } else {
     mvwprintw( window_ptr->window_ptr, 0, col, " [%s] ", window_ptr->name);
  }
}

void draw_list(struct window* window_ptr) {
	unsigned short rows, cols;
	getmaxyx(window_ptr->window_ptr, rows, cols);

	short offset = window_ptr->index + 3 - rows; // first displayed index
	if(offset < 0) offset = 0;

	unsigned short row =1, col = 1;
	JSList* node;
	for ( node=jack_slist_nth(window_ptr->list_ptr,offset); node; node=jack_slist_next(node) ) {
		char fmt[40];
		unsigned short color = (row == window_ptr->index - offset + 1)
			? (window_ptr->selected) ? 3 : 2 : 1;
		wattron(window_ptr->window_ptr, COLOR_PAIR(color));

		switch(window_ptr->type) {
		case WIN_PORTS:;
			struct port* p = node->data;
			snprintf(fmt, sizeof(fmt), "%%-%d.%ds", cols - 2, cols - 2);
			mvwprintw(window_ptr->window_ptr, row, col, fmt, p->name);
			break;
		case WIN_CONNECTIONS:;
			struct connection* c = node->data;
			snprintf(fmt, sizeof(fmt), "%%%d.%ds -> %%-%d.%ds",
				cols/2 - 3, cols/2 - 3, cols/2 - 3, cols/2 - 3);
			mvwprintw(window_ptr->window_ptr, row, col, fmt, c->out->name, c->in->name);
			break;
		default:
			ERR_OUT("Unknown WinType");
		}
		wattroff(window_ptr->window_ptr, COLOR_PAIR(color));
		wclrtoeol(window_ptr->window_ptr);
		row++;
	}
	draw_border(window_ptr);
	wrefresh(window_ptr->window_ptr);
}

void
create_window(struct window * window_ptr, int height, int width, int starty, int startx, const char * name, enum WinType type) {
//  window_ptr->list_ptr = list_ptr;
  window_ptr->window_ptr = newwin(height, width, starty, startx);
  window_ptr->selected = FALSE;
  window_ptr->width = width;
  window_ptr->height = height;
  window_ptr->name = name;
  window_ptr->index = 0;
  window_ptr->count = jack_slist_length(window_ptr->list_ptr);
  window_ptr->type = type;
//  scrollok(window_ptr->window_ptr, TRUE);
}

void
resize_window(struct window * window_ptr, int height, int width, int starty, int startx) {
//  delwin(window_ptr->window_ptr);
//  window_ptr->window_ptr = newwin(height, width, starty, startx);
  wresize(window_ptr->window_ptr, height, width);
  mvwin(window_ptr->window_ptr, starty, startx);
  window_ptr->width = width;
  window_ptr->height = height;
}

const char*
get_selected_port_name(struct window* window_ptr) {
   JSList* list = jack_slist_nth(window_ptr->list_ptr, window_ptr->index);
   if (!list) return NULL;
   struct port* p = list->data;
   return p->name;
}

bool
w_connect(jack_client_t* client, struct window* window_src_ptr, struct window* window_dst_ptr) {
   const char* src = get_selected_port_name(window_src_ptr);
   if(!src) return FALSE;
   const char* dst = get_selected_port_name(window_dst_ptr);
   if(!dst) return FALSE;

   if (jack_connect(client, src, dst) ) return FALSE;

   /* Move selections to next items */
   window_item_next(window_src_ptr);
   window_item_next(window_dst_ptr);
   return TRUE;
}

bool 
w_disconnect(jack_client_t* client, struct window* window_ptr) {
   JSList* list = jack_slist_nth(window_ptr->list_ptr, window_ptr->index);
   if (! list) return FALSE;

   struct connection* c = list->data;
   if (window_ptr->index >= window_ptr->count - 1 && window_ptr->index)
      window_ptr->index--;
   return jack_disconnect(client, c->out->name, c->in->name) ? FALSE : TRUE;
}

void free_all_ports(JSList* all_ports) {
  /* First node is pointer to calloc-ed big chunk */
  if (! all_ports) return;
  free(all_ports->data);
  jack_slist_free(all_ports);
}

void cleanup(struct window* windows) {
  short i;
  struct window* w = windows;
  JSList *l, *node;

  for(i = 0; i < 3; i++, w++) {
     l = w->list_ptr;
     if( w->type == WIN_CONNECTIONS ) {
       for ( node=w->list_ptr; node; node=jack_slist_next(node) )
          free(node->data);
     }
     jack_slist_free(l);
  }
}

unsigned short
select_window(struct window* windows, int current, int new) {
   if (new == current) {
      return current;
   } else if (new > 2) {
      new = 0;
   } else if (new < 0) {
      new = 2;
   }

   if (new == 2 && ! jack_slist_length( windows[2].list_ptr )) {
      new = (new > current) ? 0 : 1;
   }

   windows[current].selected = FALSE;
   windows[new].selected = TRUE;
   return new;
}

int graph_order_handler(void *arg) {
    struct graph *graph = arg;
    *(graph->want_refresh) = TRUE;
    *(graph->err_message) = GRAPH_CHANGED;
    return 0;
}

int process_handler ( jack_nframes_t nframes, void *arg ) {
    return 0;
}

void draw_status(WINDOW* w, int c, const char* msg, float dsp_load, bool rt) {
    unsigned short cols;
    cols = getmaxx(w);

    wmove(w, 0, 0);
    wclrtoeol(w);

    wattron(w, COLOR_PAIR(c));
    mvwprintw(w, 0, 1, msg);
    wattroff(w, COLOR_PAIR(c));

    wattron(w, COLOR_PAIR(7));
    mvwprintw(w, 0, cols-12, "DSP:%4.2f%s", dsp_load, rt ? "@RT" : "!RT" );
    wattroff(w, COLOR_PAIR(7));

    wrefresh(w);
}

int get_max_port_name ( JSList* list ) {
    int ret = 0;
    JSList* node;
    for ( node=list; node; node=jack_slist_next(node) ) {
        struct port* p = node->data;
        int len = strlen ( p->name );
        if ( len > ret ) ret = len;
    }
    return ret;
}

enum Orientation { ORT_VERT, ORT_HORIZ };
void grid_draw_port_list ( WINDOW* w, JSList* list, int start, enum Orientation ort ) {
	unsigned short rows, cols;
	getmaxyx(w, rows, cols);

	int row, col;
	if ( ort == ORT_VERT ) {
		row = 1; col = start;
		mvwvline(w, row, col, ACS_VLINE, rows);
	} else { /* assume ORT_HORIZ */
		row = start; col = 1;
		mvwhline(w, row, col, ACS_HLINE, cols);
	}

	JSList* node;
	for ( node=jack_slist_nth(list,0); node; node=jack_slist_next(node) ) {
		struct port* p = node->data;

		/* Draw port name */
		wattron(w, COLOR_PAIR(1));
		if ( ort == ORT_VERT ) {
			mvwprintw(w, row++, ++col, "%s", p->name);
		} else { /* assume ORT_HORIZ */
			mvwprintw(w, ++row, col, "%s", p->name);
		}
		wattroff(w, COLOR_PAIR(1));

		/* Draw line */
		if ( ort == ORT_VERT ) {
			mvwvline(w, row, ++col, ACS_VLINE, rows);
		} else { /* assume ORT_HORIZ */
			mvwhline(w, ++row, col, ACS_HLINE, cols);
		}
	}
}

void draw_grid ( WINDOW* w, JSList* list_out, JSList* list_in, JSList* list_con ) {
	wclear ( w );

	/* IN */
	int start_col = get_max_port_name ( list_out ) + 1;
	grid_draw_port_list ( w, list_in, start_col, ORT_VERT );

	/* OUT */
	int start_row = jack_slist_length( list_in ) + 1;
	grid_draw_port_list ( w, list_out, start_row, ORT_HORIZ );

	/* Draw Connections */
	JSList* node;
	for ( node=jack_slist_nth(list_con,0); node; node=jack_slist_next(node) ) {
		struct connection* c = node->data;
        
		int in_pos = jack_slist_find_pos ( list_in, c->in );
		int col = start_col + 1 + in_pos * 2;

		int out_pos = jack_slist_find_pos ( list_out, c->out );
		int row = start_row + 1 + out_pos * 2;

		wattron(w, COLOR_PAIR(2));
		mvwprintw(w, row, col, "%c", 'X' );
		wattroff(w, COLOR_PAIR(2));
	}

	/* Draw border */
	wattron(w, COLOR_PAIR(1));
	box(w, 0, 0);
	wattroff(w, COLOR_PAIR(1));

	wrefresh(w);
}

struct help {
	const char* keys;
	const char* action;
};

void show_help() {
    unsigned short rows, cols;
    getmaxyx(stdscr, rows, cols);

    WINDOW* w = newwin(rows , cols, 0, 0);

    struct help h[] = {
       { "a", "manage audio" },
       { "m", "manage MIDI" },
       { "g", "Toggle grid view" },
       { "TAB / SHIFT + j", "select next window" },
       { "SHIFT + TAB / K", "select previous window" },
       { "SPACE", "select connections window" },
       { "LEFT / h", "select output ports window" },
       { "RIGHT / j", "select input ports window" },
       { "UP / k", "select previous item on list" },
       { "DOWN / j", "select previous item on list" },
       { "HOME", "select first item on list" },
       { "END", "select last item on list" },
       { "c / ENTER", "connect" },
       { "d / BACKSPACE", "disconnect" },
       { "SHIFT + d", "disconnect all" },
       { "r", "refresh" },
       { "q", "quit" },
       { "SHIFT + h / ?", "help info (just what you see right now ;-)" },
       { NULL, NULL }
    };

    wattron(w, COLOR_PAIR(6));
    wprintw( w, "\n" );
    wprintw ( w, "          _                                _\n");  
    wprintw ( w, "   _ _   (_) __  ___  _ _   _ _   ___  __ | |_\n");
    wprintw ( w, "  | ' \\  | |/ _|/ _ \\| ' \\ | ' \\ / -_)/ _||  _|\n");
    wprintw ( w, "  |_||_|_/ |\\__|\\___/|_||_||_||_|\\___|\\__| \\__|\n");
    wprintw ( w, "       |__/ version %s by Xj\n", VERSION);
    wattroff(w, COLOR_PAIR(6));

    struct help* hh;
    for (hh = h; hh->keys; hh++)
       wprintw( w, "  %15s - %s\n", hh->keys, hh->action );

    wattron(w, COLOR_PAIR(1));
    box(w, 0, 0);
    wattroff(w, COLOR_PAIR(1));

    wrefresh(w);
    wgetch(w);
    delwin(w);
}

enum ViewMode { VIEW_MODE_NORMAL, VIEW_MODE_GRID };
int main() {
  unsigned short i, ret, rows, cols, window_selection=0;
  struct window windows[3];
  WINDOW* status_window;
  WINDOW* grid_window = NULL;
  const char* err_message = NULL;
  enum ViewMode ViewMode = VIEW_MODE_NORMAL;
  const char* PortsType = JACK_DEFAULT_MIDI_TYPE;
  JSList *all_list = NULL;
  bool want_refresh = FALSE;
  struct graph g = { &want_refresh, &err_message };

  /* Initialize ncurses */
  initscr();
  curs_set(0); /* set cursor invisible */
  noecho();
  getmaxyx(stdscr, rows, cols);

  if (has_colors() == FALSE) {
    ERR_OUT("Your terminal does not support color");
    ret = -1;
    goto qxit;
  }

  start_color();
  use_default_colors();
  init_pair(1, COLOR_CYAN, -1);
  init_pair(2, COLOR_BLACK, COLOR_WHITE);
  init_pair(3, COLOR_BLACK, COLOR_GREEN);
  init_pair(4, COLOR_WHITE, -1);
  init_pair(5, COLOR_BLACK, COLOR_RED);
  init_pair(6, COLOR_YELLOW, -1);
  init_pair(7, COLOR_BLUE, -1);

  /* Create Help Window */
  status_window = newwin(WSTAT_H, WSTAT_W, WSTAT_Y, WSTAT_X);
  keypad(status_window, TRUE);
  wtimeout(status_window, 3000);

  /* Some Jack versions are very aggressive in breaking view */
  jack_set_info_function(suppress_jack_log);
  jack_set_error_function(suppress_jack_log);

  /* Initialize jack */
  jack_status_t status;
  jack_client_t* client = jack_client_open (APPNAME, JackNoStartServer, &status);
  if (! client) {
    if (status & JackServerFailed) ERR_OUT ("JACK server not running");
    else ERR_OUT ("jack_client_open() failed, status = 0x%2.0x", status);
    ret = 2;
    goto quit_no_clean;
  }

  bool rt = jack_is_realtime(client);
  jack_set_graph_order_callback(client, graph_order_handler, &g);

  /* NOTE: need minimal process callback for Jack1 to call graph order handler */
  jack_set_process_callback ( client, process_handler, NULL );

  jack_activate(client);

  /* Build ports, connections list */
  all_list = build_ports(client);
  windows[0].list_ptr = select_ports(all_list, JackPortIsOutput, PortsType);
  windows[1].list_ptr = select_ports(all_list, JackPortIsInput, PortsType);
  windows[2].list_ptr = build_connections(client, all_list, PortsType);

  /* Create windows */
  create_window(windows, WOUT_H, WOUT_W, WOUT_Y, WOUT_Y, "Output Ports", WIN_PORTS);
  create_window(windows+1, WIN_H, WIN_W, WIN_Y, WIN_X, "Input Ports", WIN_PORTS);
  create_window(windows+2, WCON_H, WCON_W, WCON_Y, WCON_X, CON_NAME_M, WIN_CONNECTIONS);
  windows[window_selection].selected = TRUE;

loop:
	if ( ViewMode == VIEW_MODE_GRID ) {
		draw_grid( grid_window, windows[0].list_ptr, windows[1].list_ptr, windows[2].list_ptr );
	} else { /* Assume VIEW_MODE_NORMAL */
		for (i=0; i < 3; i++) draw_list(windows+i);
	}

	if (err_message) {
		draw_status(status_window, 5, err_message, jack_cpu_load(client), rt);
		err_message = NULL;
	} else {
		draw_status(status_window, 6, DEFAULT_STATUS, jack_cpu_load(client), rt);
	}

	int c = wgetch(status_window);

	/* Common keys */
	switch ( c ) {
	case 'g': /* Toggle grid */
		if ( ViewMode == VIEW_MODE_GRID ) {
			ViewMode = VIEW_MODE_NORMAL;
			delwin(grid_window);
			grid_window = NULL;
		} else { /* Assume VIEW_MODE_NORMAL */
			ViewMode = VIEW_MODE_GRID;
			unsigned short rows, cols;
			getmaxyx(stdscr, rows, cols);
			grid_window = newwin(rows - 1, cols, 0, 0);
		}
		goto refresh;
	case 'a': /* Show Audio Ports */
		windows[2].name = CON_NAME_A;
		PortsType = JACK_DEFAULT_AUDIO_TYPE;
		goto refresh;
	case 'm': /* Show MIDI Ports */
		windows[2].name = CON_NAME_M;
		PortsType = JACK_DEFAULT_MIDI_TYPE;
		goto refresh;
	case 'q': /* Quit from app */
	case KEY_EXIT: 
		ret =0;
		goto quit;
	
	case 'r': /* Force refresh or terminal resize */
	case KEY_RESIZE:
		getmaxyx(stdscr, rows, cols);
		wresize(status_window, WSTAT_H, WSTAT_W);
		mvwin(status_window, WSTAT_Y, WSTAT_X);
		resize_window(windows, WOUT_H, WOUT_W, WOUT_Y, WOUT_X);
		resize_window(windows+1, WIN_H, WIN_W, WIN_Y, WIN_X);
		resize_window(windows+2, WCON_H, WCON_W, WCON_Y, WCON_X);

		if ( ViewMode == VIEW_MODE_GRID )
			wresize(grid_window, rows - 1, cols);
		goto refresh;
	case '?': /* Help */
	case 'H':
		show_help();
		goto refresh;
	}

	/* Normal mode keys */
	switch ( c ) {
	case 'J': /* Select Next window */
	case KEY_TAB:
		window_selection = select_window(windows, window_selection, window_selection+1);
		goto loop;
	case 'K': /* Select Previous window */
	case KEY_BTAB:
		window_selection = select_window(windows, window_selection, window_selection-1);
		goto loop;
	case 'c': /* Connect */
	case '\n':
	case KEY_ENTER:
		if ( w_connect(client, windows, windows+1) ) goto refresh;
			err_message = ERR_CONNECT;
		goto loop;
	case 'd': /* Disconnect */
	case KEY_BACKSPACE:
		if (w_disconnect(client, windows+2) ) goto refresh;
			err_message = ERR_DISCONNECT;
		goto loop;
	case 'D': /* Disconnect all */
		while (w_disconnect(client, windows+2) ) {
			windows[2].list_ptr = jack_slist_remove(windows[2].list_ptr,
			jack_slist_nth(windows[2].list_ptr, windows[2].index)->data);
			windows[2].count--;
		}
		goto refresh;
	case 'j': /* Select next item on list */
	case KEY_DOWN:
		window_item_next(windows+window_selection);
		goto loop;
	case KEY_UP: /* Select previous item on list */
	case 'k':
		window_item_previous(windows+window_selection);
		goto loop;
	case KEY_HOME: /* Select first item on list */
		(windows+window_selection)->index = 0;
		goto loop;
	case KEY_END: /* Select last item on list */
		(windows+window_selection)->index = (windows+window_selection)->count - 1;
		goto loop;
	case 'h': /* Select left window */
	case KEY_LEFT:
		window_selection = select_window(windows, window_selection, 0);
		goto loop;
	case 'l': /* Select right window */
	case KEY_RIGHT:
		window_selection = select_window(windows, window_selection, 1);
		goto loop;
	case ' ': /* Select bottom window */
		window_selection = select_window(windows, window_selection, 2);
		goto loop;
  	}

	if (! want_refresh) goto loop;
refresh:
	want_refresh = FALSE;
	free_all_ports(all_list);
	cleanup(windows); /* Clean windows lists */

	all_list = build_ports(client);
	windows[0].list_ptr = select_ports(all_list, JackPortIsOutput, PortsType);
	windows[1].list_ptr = select_ports(all_list, JackPortIsInput, PortsType);
	windows[2].list_ptr = build_connections(client, all_list, PortsType);

	if ( ViewMode == VIEW_MODE_NORMAL ) {
		for(i=0; i < 3; i++) {
			windows[i].count = jack_slist_length( windows[i].list_ptr );
			if (windows[i].index > windows[i].count - 1) windows[i].index = 0;
			wclear(windows[i].window_ptr);
		}
	}
	goto loop;
quit:
	free_all_ports(all_list);
	cleanup(windows); /* Clean windows lists */
quit_no_clean:
	jack_deactivate(client);
	jack_client_close (client);
qxit:
	endwin();
	return ret;
}
