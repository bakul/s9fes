/*
 * Scheme 9 from Empty Space, Curses Interface
 * By Nils M Holm, 2010-2015
 * Placed in the Public Domain
 *
 * A low-level interface to some CURSES(3) routines.
 */

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"

/*
 * XXX The C "macro" preprocessor does not allow to do this
 * in a more consistent way, so the following values have to
 * be *copied* from s9core.h. *Sigh*
 */

#define S9_TRUE  (-2)
#define S9_FALSE (-3)
#define _nl() pr("\n")
#undef nl
#undef TRUE
#undef FALSE

#include <stdlib.h>
#include <curses.h>

static int	Curses_running = 0;

cell pp_curs_addch(void) {
	if (!Curses_running) return UNSPECIFIC;
	addch(char_value(parg(1)));
	return UNSPECIFIC;
}

cell pp_curs_addstr(void) {
	if (!Curses_running) return UNSPECIFIC;
	addstr(string(parg(1)));
	return UNSPECIFIC;
}

cell pp_curs_attrset(void) {
	if (!Curses_running) return UNSPECIFIC;
	attrset(integer_value("curs:attrset", parg(1)));
	return UNSPECIFIC;
}

cell pp_curs_beep(void) {
	if (!Curses_running) return UNSPECIFIC;
	beep();
	return UNSPECIFIC;
}

cell pp_curs_cbreak(void) {
	if (!Curses_running) return UNSPECIFIC;
	cbreak();
	return UNSPECIFIC;
}

cell pp_curs_clear(void) {
	if (!Curses_running) return UNSPECIFIC;
	clear();
	return UNSPECIFIC;
}

cell pp_curs_clearok(void) {
	if (!Curses_running) return UNSPECIFIC;
	clearok(stdscr, parg(1) == S9_TRUE? TRUE: FALSE);
	return UNSPECIFIC;
}

cell pp_curs_clrtobot(void) {
	if (!Curses_running) return UNSPECIFIC;
	clrtobot();
	return UNSPECIFIC;
}

cell pp_curs_clrtoeol(void) {
	if (!Curses_running) return UNSPECIFIC;
	clrtoeol();
	return UNSPECIFIC;
}

cell pp_curs_cols(void) {
	return make_integer(COLS);
}

cell pp_curs_cursoff(void) {
	if (!Curses_running) return UNSPECIFIC;
	curs_set(0);
	return UNSPECIFIC;
}

cell pp_curs_curson(void) {
	if (!Curses_running) return UNSPECIFIC;
	curs_set(1);
	return UNSPECIFIC;
}

cell pp_curs_delch(void) {
	if (!Curses_running) return UNSPECIFIC;
	delch();
	return UNSPECIFIC;
}

cell pp_curs_deleteln(void) {
	if (!Curses_running) return UNSPECIFIC;
	deleteln();
	return UNSPECIFIC;
}

cell pp_curs_echo(void) {
	if (!Curses_running) return UNSPECIFIC;
	echo();
	return UNSPECIFIC;
}

cell pp_curs_endwin(void) {
	if (!Curses_running) return UNSPECIFIC;
	endwin();
	Curses_running = 0;
	return UNSPECIFIC;
}

cell pp_curs_flash(void) {
	if (!Curses_running) return UNSPECIFIC;
	flash();
	return UNSPECIFIC;
}

cell pp_curs_flushinp(void) {
	if (!Curses_running) return UNSPECIFIC;
	flushinp();
	return UNSPECIFIC;
}

cell pp_curs_get_magic_value(void) {
	char	*s = string(parg(1));

	if (!strcmp(s, "A_BOLD")) return make_integer(A_BOLD);
	if (!strcmp(s, "A_NORMAL")) return make_integer(A_NORMAL);
	if (!strcmp(s, "A_STANDOUT")) return make_integer(A_STANDOUT);
	if (!strcmp(s, "A_UNDERLINE")) return make_integer(A_UNDERLINE);
	if (!strcmp(s, "KEY_BACKSPACE")) return make_integer(KEY_BACKSPACE);
	if (!strcmp(s, "KEY_DC")) return make_integer(KEY_DC);
	if (!strcmp(s, "KEY_DOWN")) return make_integer(KEY_DOWN);
	if (!strcmp(s, "KEY_END")) return make_integer(KEY_END);
	if (!strcmp(s, "KEY_IC")) return make_integer(KEY_IC);
	if (!strcmp(s, "KEY_HOME")) return make_integer(KEY_HOME);
	if (!strcmp(s, "KEY_LEFT")) return make_integer(KEY_LEFT);
	if (!strcmp(s, "KEY_NPAGE")) return make_integer(KEY_NPAGE);
	if (!strcmp(s, "KEY_PPAGE")) return make_integer(KEY_PPAGE);
	if (!strcmp(s, "KEY_RIGHT")) return make_integer(KEY_RIGHT);
	if (!strcmp(s, "KEY_UP")) return make_integer(KEY_UP);
	error("curs:get-magic-value: requested value not found",
		parg(1));
	return UNDEFINED;
}

cell pp_curs_getch(void) {
	int	c;

	if (!Curses_running) return UNSPECIFIC;
	c = getch();
	if (c == ERR)
		return S9_FALSE;
	return make_integer(c);
}

cell pp_curs_getyx(void) {
	int	cx, cy;
	cell	n;

	if (!Curses_running) return UNSPECIFIC;
	getyx(stdscr, cy, cx);
	n = make_integer(cx);
	n = cons(n, NIL);
	save(n);
	n = cons(make_integer(cy), n);
	unsave(1);
	return n;
}

cell pp_curs_idlok(void) {
	if (!Curses_running) return UNSPECIFIC;
	idlok(stdscr, parg(1) == S9_TRUE? TRUE: FALSE);
	return UNSPECIFIC;
}

cell pp_curs_inch(void) {
	if (!Curses_running) return UNSPECIFIC;
	return make_char(inch());
}

cell pp_curs_insch(void) {
	if (!Curses_running) return UNSPECIFIC;
	insch(char_value(parg(1)));
	return UNSPECIFIC;
}

#ifdef CURSES_COLOR

cell pp_curs_initscr(void) {
	int colors[] = { COLOR_BLACK, COLOR_BLUE, COLOR_GREEN,
			 COLOR_CYAN, COLOR_RED, COLOR_MAGENTA,
			 COLOR_YELLOW, COLOR_WHITE };
	int	f, b;

	if (Curses_running) return UNSPECIFIC;
	initscr();
	start_color();
	for (b=0; b<8; b++) {
		for (f=0; f<8; f++) {
			init_pair(b*8+f, colors[f], colors[b]);
		}
	}
	Curses_running = 1;
	return UNSPECIFIC;
}

cell pp_curs_color_set(void) {
	int	f, b;
	char	name[] = "curs:color-set";

	f = integer_value(name, parg(1));
	b = integer_value(name, parg(2));
	color_set(b<<3|f, NULL);
	return UNSPECIFIC;
}

cell pp_curs_has_colors(void) {
	return has_colors()? S9_TRUE: S9_FALSE;
}

#else /* !CURSES_COLOR */

cell pp_curs_has_colors(void) {
	return S9_FALSE;
}
cell pp_curs_initscr(void) {
	if (Curses_running) return UNSPECIFIC;
	initscr();
	Curses_running = 1;
	return UNSPECIFIC;
}

#endif /* !CURSES_COLOR */

cell pp_curs_insertln(void) {
	if (!Curses_running) return UNSPECIFIC;
	insertln();
	return UNSPECIFIC;
}

cell pp_curs_keypad(void) {
	if (!Curses_running) return UNSPECIFIC;
	keypad(stdscr, parg(1) == S9_TRUE? TRUE: FALSE);
	return UNSPECIFIC;
}

cell pp_curs_lines(void) {
	return make_integer(LINES);
}

cell pp_curs_move(void) {
	char	name[] = "curs:move";

	if (!Curses_running) return UNSPECIFIC;
	move(integer_value(name, parg(1)), integer_value(name, parg(2)));
	return UNSPECIFIC;
}

cell pp_curs_mvaddch(void) {
	char	name[] = "curs:mvaddch";

	if (!Curses_running) return UNSPECIFIC;
	mvaddch(integer_value(name, parg(1)),
		integer_value(name, parg(2)),
		char_value(parg(3)));
	return UNSPECIFIC;
}

cell pp_curs_mvaddstr(void) {
	char	name[] = "curs:mvaddstr";

	if (!Curses_running) return UNSPECIFIC;
	mvaddstr(integer_value(name, parg(1)),
		integer_value(name, parg(2)),
		string(parg(3)));
	return UNSPECIFIC;
}

cell pp_curs_mvcur(void) {
	char	name[] = "curs:mvcur";

	if (!Curses_running) return UNSPECIFIC;
	if (!integer_p(parg(4)))
		error("curs:mvcur: expected integer, got", parg(4));
	mvcur(integer_value(name, parg(1)),
		integer_value(name, parg(2)),
		integer_value(name, parg(3)),
		integer_value(name, parg(4)));
	return UNSPECIFIC;
}

cell pp_curs_mvdelch(void) {
	char	name[] = "curs:mvdelch";

	if (!Curses_running) return UNSPECIFIC;
	mvdelch(integer_value(name, parg(1)),
		integer_value(name, parg(2)));
	return UNSPECIFIC;
}

cell pp_curs_mvgetch(void) {
	char	name[] = "curs:mvgetch";
	int	c;

	if (!Curses_running) return UNSPECIFIC;
	c = mvgetch(integer_value(name, parg(1)),
			integer_value(name, parg(2)));
	if (c == ERR) return S9_FALSE;
	return make_integer(c);
}

cell pp_curs_mvinch(void) {
	char	name[] = "curs:mvinch";

	if (!Curses_running) return UNSPECIFIC;
	return make_char((int) mvinch(integer_value(name, parg(1)),
			integer_value(name, parg(2))));
}

cell pp_curs_mvinsch(void) {
	char	name[] = "curs:mvinsch";

	if (!Curses_running) return UNSPECIFIC;
	mvinsch(integer_value(name, parg(1)),
		integer_value(name, parg(2)),
		char_value(parg(3)));
	return UNSPECIFIC;
}

cell pp_curs_nl(void) {
	if (!Curses_running) return UNSPECIFIC;
	nl();
	return UNSPECIFIC;
}

cell pp_curs_nocbreak(void) {
	if (!Curses_running) return UNSPECIFIC;
	nocbreak();
	return UNSPECIFIC;
}

cell pp_curs_nodelay(void) {
	if (!Curses_running) return UNSPECIFIC;
	nodelay(stdscr, parg(1) == S9_TRUE? TRUE: FALSE);
	return UNSPECIFIC;
}

cell pp_curs_noecho(void) {
	if (!Curses_running) return UNSPECIFIC;
	noecho();
	return UNSPECIFIC;
}

cell pp_curs_nonl(void) {
	if (!Curses_running) return UNSPECIFIC;
	nonl();
	return UNSPECIFIC;
}

cell pp_curs_noraw(void) {
	if (!Curses_running) return UNSPECIFIC;
	noraw();
	return UNSPECIFIC;
}

cell pp_curs_raw(void) {
	if (!Curses_running) return UNSPECIFIC;
	raw();
	return UNSPECIFIC;
}

cell pp_curs_refresh(void) {
	if (!Curses_running) return UNSPECIFIC;
	refresh();
	return UNSPECIFIC;
}

cell pp_curs_resetty(void) {
	if (!Curses_running) return UNSPECIFIC;
	resetty();
	return UNSPECIFIC;
}

cell pp_curs_savetty(void) {
	if (!Curses_running) return UNSPECIFIC;
	savetty();
	return UNSPECIFIC;
}

cell pp_curs_scroll(void) {
	if (!Curses_running) return UNSPECIFIC;
	scrl(integer_value("curs:scroll", parg(1)));
	return UNSPECIFIC;
}

cell pp_curs_scrollok(void) {
	if (!Curses_running) return UNSPECIFIC;
	scrollok(stdscr, parg(1) == S9_TRUE? TRUE: FALSE);
	return UNSPECIFIC;
}

cell pp_curs_unctrl(void) {
	char	*s;

	if (!Curses_running) return UNSPECIFIC;
	s = (char *) unctrl(integer_value("curs:unctrl", parg(1)));
	return make_string(s, strlen(s));
}

cell pp_curs_ungetch(void) {
	if (!Curses_running) return UNSPECIFIC;
	ungetch(integer_value("curs:ungetch", parg(1)));
	return UNSPECIFIC;
}

S9_PRIM Curs_primitives[] = {
 { "curs:addch",            pp_curs_addch,           1,  1, { CHR,___,___ } },
 { "curs:addstr",           pp_curs_addstr,          1,  1, { STR,___,___ } },
 { "curs:attrset",          pp_curs_attrset,         1,  1, { INT,___,___ } },
 { "curs:beep",             pp_curs_beep,            0,  0, { ___,___,___ } },
 { "curs:cbreak",           pp_curs_cbreak,          0,  0, { ___,___,___ } },
 { "curs:clear",            pp_curs_clear,           0,  0, { ___,___,___ } },
 { "curs:clearok",          pp_curs_clearok,         1,  1, { BOL,___,___ } },
 { "curs:clrtobot",         pp_curs_clrtobot,        0,  0, { ___,___,___ } },
 { "curs:clrtoeol",         pp_curs_clrtoeol,        0,  0, { ___,___,___ } },
#ifdef CURSES_COLOR
 { "curs:color-set",        pp_curs_color_set,       2,  2, { INT,INT,___ } },
#endif /* CURSES_COLOR */
 { "curs:cols",             pp_curs_cols,            0,  0, { ___,___,___ } },
 { "curs:cursoff",          pp_curs_cursoff,         0,  0, { ___,___,___ } },
 { "curs:curson",           pp_curs_curson,          0,  0, { ___,___,___ } },
 { "curs:delch",            pp_curs_delch,           0,  0, { ___,___,___ } },
 { "curs:deleteln",         pp_curs_deleteln,        0,  0, { ___,___,___ } },
 { "curs:echo",             pp_curs_echo,            0,  0, { ___,___,___ } },
 { "curs:endwin",           pp_curs_endwin,          0,  0, { ___,___,___ } },
 { "curs:flash",            pp_curs_flash,           0,  0, { ___,___,___ } },
 { "curs:flushinp",         pp_curs_flushinp,        0,  0, { ___,___,___ } },
 { "curs:get-magic-value",  pp_curs_get_magic_value, 1,  1, { STR,___,___ } },
 { "curs:getch",            pp_curs_getch,           0,  0, { ___,___,___ } },
 { "curs:getyx",            pp_curs_getyx,           0,  0, { ___,___,___ } },
 { "curs:has-colors",       pp_curs_has_colors,      0,  0, { ___,___,___ } },
 { "curs:idlok",            pp_curs_idlok,           1,  1, { BOL,___,___ } },
 { "curs:inch",             pp_curs_inch,            0,  0, { ___,___,___ } },
 { "curs:insch",            pp_curs_insch,           1,  1, { CHR,___,___ } },
 { "curs:initscr",          pp_curs_initscr,         0,  0, { ___,___,___ } },
 { "curs:insertln",         pp_curs_insertln,        0,  0, { ___,___,___ } },
 { "curs:keypad",           pp_curs_keypad,          1,  1, { BOL,___,___ } },
 { "curs:lines",            pp_curs_lines,           0,  0, { ___,___,___ } },
 { "curs:move",             pp_curs_move,            2,  2, { INT,INT,___ } },
 { "curs:mvaddch",          pp_curs_mvaddch,         3,  3, { INT,INT,CHR } },
 { "curs:mvaddstr",         pp_curs_mvaddstr,        3,  3, { INT,INT,STR } },
 { "curs:mvcur",            pp_curs_mvcur,           2,  2, { INT,INT,___ } },
 { "curs:mvdelch",          pp_curs_mvdelch,         2,  2, { INT,INT,___ } },
 { "curs:mvgetch",          pp_curs_mvgetch,         2,  2, { INT,INT,___ } },
 { "curs:mvinch",           pp_curs_mvinch,          2,  2, { INT,INT,___ } },
 { "curs:mvinsch",          pp_curs_mvinsch,         2,  2, { INT,INT,___ } },
 { "curs:nl",               pp_curs_nl,              0,  0, { ___,___,___ } },
 { "curs:nocbreak",         pp_curs_nocbreak,        0,  0, { ___,___,___ } },
 { "curs:nodelay",          pp_curs_nodelay,         1,  1, { BOL,___,___ } },
 { "curs:noecho",           pp_curs_noecho,          0,  0, { ___,___,___ } },
 { "curs:nonl",             pp_curs_nonl,            0,  0, { ___,___,___ } },
 { "curs:noraw",            pp_curs_noraw,           0,  0, { ___,___,___ } },
 { "curs:raw",              pp_curs_raw,             0,  0, { ___,___,___ } },
 { "curs:refresh",          pp_curs_refresh,         0,  0, { ___,___,___ } },
 { "curs:resetty",          pp_curs_resetty,         0,  0, { ___,___,___ } },
 { "curs:savetty",          pp_curs_savetty,         0,  0, { ___,___,___ } },
 { "curs:scroll",           pp_curs_scroll,          1,  1, { INT,___,___ } },
 { "curs:scrollok",         pp_curs_scrollok,        1,  1, { BOL,___,___ } },
 { "curs:unctrl",           pp_curs_unctrl,          1,  1, { INT,___,___ } },
 { "curs:ungetch",          pp_curs_ungetch,         1,  1, { INT,___,___ } },
 { NULL }
};

void curs_init(void) {
	add_primitives("curses", Curs_primitives);
}
