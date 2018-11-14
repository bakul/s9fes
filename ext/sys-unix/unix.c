/*
 * Scheme 9 from Empty Space, Unix Interface
 * By Nils M Holm, 2009-2018
 * In the public domain
 *
 * A low-level interface to some Unix system services.
 */

/*
 * Make Linux happy.
 */

#ifndef _BSD_SOURCE
 #define _BSD_SOURCE
#endif

#ifndef __FreeBSD__
 #ifndef __NetBSD__
  #ifndef __OpenBSD__

   #ifndef _POSIX_SOURCE
    #define _POSIX_SOURCE
   #endif

   #ifndef _XOPEN_SOURCE
    #define _XOPEN_SOURCE
   #endif

   #ifndef _XOPEN_SOURCE_EXTENDED
    #define _XOPEN_SOURCE_EXTENDED
   #endif

   #ifndef _DEFAULT_SOURCE
    #define _DEFAULT_SOURCE
   #endif

  #endif
 #endif
#endif

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/wait.h>
#include <sys/select.h>
#include <pwd.h>
#include <grp.h>
#include <dirent.h>
#include <signal.h>

#ifdef NETWORK
 #include <sys/socket.h>
 #include <netdb.h>
 #include <sys/utsname.h>
 #include <arpa/inet.h>
 #include <netinet/in.h>
#endif /* NETWORK */

/*
 * XXX Shouldn't these be defined in <sys/stat.h> and <signal.h>?
 * Probably just another POSIX hickup. Please someone supply a
 * clean solution.
 */

#ifndef S_ISVTX
 #define S_ISVTX	01000
#endif
#ifndef SIGTRAP
 #define SIGTRAP	5
#endif
#ifndef SIGEMT
 #define SIGEMT		7
#endif
#ifndef SIGSYS
 #define SIGSYS		12
#endif

/*
 *	Allow us at least to write
 *		assign(car(x), cons(foo, bar));
 *	in presence of that fact that C's
 *	order of evaluation messes up
 *		car(x) = cons(foo, bar);
 */

static cell		New_node;
#define assign(n,v)	do { New_node = (v); n = New_node; } while(0)

cell	Last_errno = 0;
cell	Catch_errors = 0;

cell sys_error(char *who) {
	char	buf[256];
	char	*p, *q;
	int	i, k;
	cell	a;

	Last_errno = errno;
	if (who) {
		if (Catch_errors) return FALSE;
		k = strlen(who);
		strcpy(buf, who);
		strcpy(&buf[k], ": ");
		k += 2;
		q = strerror(errno);
		for (p = &buf[k]; *q && k < 244; k++)
			*p++ = tolower((int) *q++);
		*p = 0;
		strcat(buf, ", arguments");
		save(a = NIL);
		for (i = narg(); i > 0; i--) {
			a = cons(parg(i), a);
			car(Stack) = a;
		}
		unsave(1);
		error(buf, a);
	}
	return FALSE;
}

cell sys_ok(void) {
	return Catch_errors? TRUE: UNSPECIFIC;
}

cell pp_sys_access(void) {
	return access(string(parg(1)),
		integer_value("sys:access", parg(2))) < 0? FALSE: TRUE;
}

cell pp_sys_catch_errors(void) {
	Catch_errors = parg(1) == TRUE;
	if (Catch_errors) Last_errno = 0;
	return UNSPECIFIC;
}

cell pp_sys_chdir(void) {
	if (chdir(string(parg(1))) < 0)
		return sys_error("sys:chdir");
	return sys_ok();
}

cell pp_sys_close(void) {
	if (close(integer_value("sys:close", parg(1))) < 0)
		return sys_error("sys:close");
	return sys_ok();
}

cell pp_sys_chmod(void) {
	int	r;

	r = chmod(string(parg(1)), integer_value("sys:chmod", parg(2)));
	if (r < 0) return sys_error("sys:chmod");
	return sys_ok();
}

cell pp_sys_chown(void) {
	int	r;

	r = chown(string(parg(1)),
		integer_value("sys:chown", parg(2)),
		integer_value("sys:chown", parg(3)));
	if (r < 0) return sys_error("sys:chown");
	return sys_ok();
}

cell pp_sys_command_line(void) {
	extern cell	Argv;

	return Argv;
}

cell pp_sys_creat(void) {
	int	fd;

	fd = open(string(parg(1)), O_CREAT|O_TRUNC|O_WRONLY,
			integer_value("sys:creat", parg(2)));
	if (fd < 0) return sys_error("sys:creat");
	return make_integer(fd);
}

cell pp_sys_dup(void) {
	int	r;

	r = dup(integer_value("sys:dup", parg(1)));
	if (r < 0) return sys_error("sys:dup");
	return make_integer(r);
}

cell pp_sys_dup2(void) {
	int	r;
	char	name[] = "sys:dup2";

	r = dup2(integer_value(name, parg(1)),
			integer_value(name, parg(2)));
	if (r < 0) return sys_error("sys:dup2");
	return make_integer(r);
}

cell pp_sys_errno(void) {
	int	e = Last_errno;

	Last_errno = 0;
	return make_integer(e);
}

cell pp_sys_execv(void) {
	char	**argv;
	cell	p;
	int	i;

	for (p = parg(2); p != NIL; p = cdr(p)) {
		if (!pair_p(p))
			error("sys:execv: improper list, last element is",
				p);
		if (!string_p(car(p)))
			error("sys:execv: expected list of string, got",
				car(p));
	}
	argv = malloc((length(parg(2)) + 2) * sizeof(char *));
	if (argv == NULL) return sys_error("sys:execv");
	argv[0] = string(parg(1));
	i = 1;
	for (p = parg(2); p != NIL; p = cdr(p))
		argv[i++] = string(car(p));
	argv[i] = NULL;
	execv(string(parg(1)), argv);
	free(argv);
	return sys_error("sys:execv");
}

cell pp_sys_exit(void) {
	int	r;

	r = integer_value("sys:exit", parg(1));
	if (r > 255 || r < 0)
		error("sys:exit: value out of range", parg(1));
	exit(r);
	fatal("sys:exit() failed");
	return sys_ok();
}

cell pp_sys_fileno(void) {
	if (!input_port_p(parg(1)) && !output_port_p(parg(1)))
		error("sys:fileno: expected port, got", parg(1));
	if (Ports[port_no(parg(1))] == NULL)
		error("sys:fileno: port not open", parg(1));
	return make_integer(fileno(Ports[port_no(parg(1))]));
}

cell pp_sys_flush(void) {
	if (fflush(Ports[port_no(parg(1))]))
		return sys_error("sys:flush");
	return sys_ok();
}

cell pp_sys_fork(void) {
	int	pid;

	pid = fork();
	if (pid < 0) return sys_error("sys:fork");
	return make_integer(pid);
}

cell pp_sys_getcwd(void) {
	char	*s;
	cell	n;

	s = getcwd(NULL, 1024);
	n = make_string(s, strlen(s));
	free(s);
	return n;
}

cell pp_sys_getenv(void) {
	char	*s;

	s = getenv(string(parg(1)));
	if (NULL == s) return FALSE;
	return make_string(s, strlen(s));
}

cell pp_sys_getgid(void) {
	return make_integer(getgid());
}

cell mkgrent(struct group *gr) {
	cell	n, a;

	n = cons(NIL, NIL);
	save(n);
	assign(car(n), cons(symbol_ref("name"), NIL));
	assign(cdar(n), make_string(gr->gr_name, strlen(gr->gr_name)));
	a = cons(NIL, NIL);
	cdr(n) = a;
	assign(car(a), cons(symbol_ref("gid"), NIL));
	assign(cdar(a), make_integer(gr->gr_gid));
	unsave(1);
	return n;
}

cell pp_sys_getgrnam(void) {
	struct group	*gr;

	gr = getgrnam(string(parg(1)));
	if (gr == NULL) return FALSE;
	return mkgrent(gr);
}

cell pp_sys_getgrgid(void) {
	struct group	*gr;

	gr = getgrgid(integer_value("sys:getgrgid", parg(1)));
	if (gr == NULL) return FALSE;
	return mkgrent(gr);
}

cell pp_sys_getpgid(void) {
	/* No prototype, neither on FreeBSD 8.2 nor on Debian Lenny? */
	pid_t	getpgid(pid_t);

	return make_integer(getpgid(0));
}

cell pp_sys_getpid(void) {
	return make_integer(getpid());
}

cell pp_sys_getpwent(void) {
	struct passwd	*pw;
	cell		n, a, pa;

	setpwent();
	n = cons(NIL, NIL);
	save(n);
	a = n;
	pa = n;
	while (1) {
		pw = getpwent();
		if (pw == NULL)
			break;
		pa = a;
		assign(car(a), make_string(pw->pw_name, strlen(pw->pw_name)));
		if (pw != NULL) {
			assign(cdr(a), cons(NIL, NIL));
			a = cdr(a);
		}
	}
	cdr(pa) = NIL;
	endpwent();
	unsave(1);
	return n;
}

cell mkpwent(struct passwd *pw) {
	cell	n, a;

	n = cons(NIL, NIL);
	save(n);
	assign(car(n), cons(symbol_ref("name"), NIL));
	assign(cdar(n), make_string(pw->pw_name, strlen(pw->pw_name)));
	a = cons(NIL, NIL);
	cdr(n) = a;
	assign(car(a), cons(symbol_ref("uid"), NIL));
	assign(cdar(a), make_integer(pw->pw_uid));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("gid"), NIL));
	assign(cdar(a), make_integer(pw->pw_gid));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("gecos"), NIL));
	assign(cdar(a), make_string(pw->pw_gecos, strlen(pw->pw_gecos)));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("home"), NIL));
	assign(cdar(a), make_string(pw->pw_dir, strlen(pw->pw_dir)));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("shell"), NIL));
	assign(cdar(a), make_string(pw->pw_shell, strlen(pw->pw_shell)));
	unsave(1);
	return n;
}

cell pp_sys_getpwnam(void) {
	struct passwd	*pw;

	pw = getpwnam(string(parg(1)));
	if (pw == NULL) return FALSE;
	return mkpwent(pw);
}

cell pp_sys_getpwuid(void) {
	struct passwd	*pw;

	pw = getpwuid(integer_value("sys:getpwuid", parg(1)));
	if (pw == NULL) return FALSE;
	return mkpwent(pw);
}

cell pp_sys_getuid(void) {
	return make_integer(getuid());
}

cell pp_sys_kill(void) {
	char	name[] = "sys:kill";
	int	sig = integer_value(name, parg(2));
	int	r;

	r = kill(integer_value(name, parg(1)), sig);
	if (r < 0) return sys_error("sys:kill");
	return sys_ok();
}

cell pp_sys_link(void) {
	if (link(string(parg(1)), string(parg(2))) < 0)
		return sys_error("sys:link");
	return sys_ok();
}

cell pp_sys_lock(void) {
	char	p[256], *s;

	s = string(parg(1));
	if (strlen(s) > 248)
		error("sys:lock: path too long", parg(1));
	sprintf(p, "%s.lock", s);
	return (mkdir(p, 0700) < 0)? FALSE: TRUE;
}

cell pp_sys_lseek(void) {
	char	name[] = "sys:lseek";
	long	r;

	r = lseek(integer_value(name, parg(1)),
		integer_value(name, parg(2)),
		integer_value(name, parg(3)));
	if (r < 0L) return sys_error("sys:lseek");
	return make_integer(r);
}

cell pp_sys_get_magic_value(void) {
	char	*s = string(parg(1));

	if (!strcmp(s, "F_OK")) return make_integer(F_OK);
	if (!strcmp(s, "X_OK")) return make_integer(X_OK);
	if (!strcmp(s, "W_OK")) return make_integer(W_OK);
	if (!strcmp(s, "R_OK")) return make_integer(R_OK);
	if (!strcmp(s, "O_RDONLY")) return make_integer(O_RDONLY);
	if (!strcmp(s, "O_WRONLY")) return make_integer(O_WRONLY);
	if (!strcmp(s, "O_RDWR")) return make_integer(O_RDWR);
	if (!strcmp(s, "SEEK_SET")) return make_integer(SEEK_SET);
	if (!strcmp(s, "SEEK_CUR")) return make_integer(SEEK_CUR);
	if (!strcmp(s, "SEEK_END")) return make_integer(SEEK_END);
	if (!strcmp(s, "SIGHUP")) return make_integer(SIGHUP);
	if (!strcmp(s, "SIGINT")) return make_integer(SIGINT);
	if (!strcmp(s, "SIGQUIT")) return make_integer(SIGQUIT);
	if (!strcmp(s, "SIGILL")) return make_integer(SIGILL);
	if (!strcmp(s, "SIGTRAP")) return make_integer(SIGTRAP);
	if (!strcmp(s, "SIGABRT")) return make_integer(SIGABRT);
	if (!strcmp(s, "SIGEMT")) return make_integer(SIGEMT);
	if (!strcmp(s, "SIGFPE")) return make_integer(SIGFPE);
	if (!strcmp(s, "SIGKILL")) return make_integer(SIGKILL);
	if (!strcmp(s, "SIGBUS")) return make_integer(SIGBUS);
	if (!strcmp(s, "SIGSEGV")) return make_integer(SIGSEGV);
	if (!strcmp(s, "SIGSYS")) return make_integer(SIGSYS);
	if (!strcmp(s, "SIGPIPE")) return make_integer(SIGPIPE);
	if (!strcmp(s, "SIGALRM")) return make_integer(SIGALRM);
	if (!strcmp(s, "SIGTERM")) return make_integer(SIGTERM);
	if (!strcmp(s, "S_ISUID")) return make_integer(S_ISUID);
	if (!strcmp(s, "S_ISGID")) return make_integer(S_ISGID);
	if (!strcmp(s, "S_ISVTX")) return make_integer(S_ISVTX);
	if (!strcmp(s, "S_IRUSR")) return make_integer(S_IRUSR);
	if (!strcmp(s, "S_IRWXU")) return make_integer(S_IRWXU);
	if (!strcmp(s, "S_IWUSR")) return make_integer(S_IWUSR);
	if (!strcmp(s, "S_IXUSR")) return make_integer(S_IXUSR);
	if (!strcmp(s, "S_IRWXG")) return make_integer(S_IRWXG);
	if (!strcmp(s, "S_IRGRP")) return make_integer(S_IRGRP);
	if (!strcmp(s, "S_IWGRP")) return make_integer(S_IWGRP);
	if (!strcmp(s, "S_IXGRP")) return make_integer(S_IXGRP);
	if (!strcmp(s, "S_IRWXO")) return make_integer(S_IRWXO);
	if (!strcmp(s, "S_IROTH")) return make_integer(S_IROTH);
	if (!strcmp(s, "S_IWOTH")) return make_integer(S_IWOTH);
	if (!strcmp(s, "S_IXOTH")) return make_integer(S_IXOTH);
	else error("sys:get-magic-value: requested value not found",
			parg(1));
	return UNDEFINED;
}

cell pp_sys_make_input_port(void) {
	int	in = new_port();

	if (in < 0) error("sys:make-input-port: out of ports", VOID);
	Ports[in] = fdopen(integer_value("sys:make-input-port", parg(1)),
				"r");
	return make_port(in, T_INPUT_PORT);
}

cell pp_sys_make_output_port(void) {
	int	out = new_port();

	if (out < 0) error("sys:make-output-port: out of ports", VOID);
	Ports[out] = fdopen(integer_value("sys:make-output-port", parg(1)),
				"w");
	return make_port(out, T_OUTPUT_PORT);
}

cell pp_sys_mkdir(void) {
	if (mkdir(string(parg(1)), integer_value("sys:mkdir", parg(2))) < 0)
		return sys_error("sys:mkdir");
	return sys_ok();
}

cell pp_sys_open(void) {
	int	fd;

	fd = open(string(parg(1)), integer_value("sys:open", parg(2)));
	if (fd < 0) return sys_error("sys:open");
	return make_integer(fd);
}

cell pp_sys_pipe(void) {
	int	fd[2];
	cell	n;

	if (pipe(fd) < 0) return sys_error("sys:pipe");
	n = cons(make_integer(fd[1]), NIL);
	save(n);
	n = cons(make_integer(fd[0]), n);
	unsave(1);
	return n;
}

cell pp_sys_read(void) {
	cell	buf, buf2;
	int	r, k;
	char	name[] = "sys:read";

	k = integer_value(name, parg(2));
	buf = make_string("", k);
	r = read(integer_value(name, parg(1)), string(buf), k);
	if (r < 0)
		return sys_error("sys:read");
	if (r < k) {
		save(buf);
		buf2 = make_string("", r);
		unsave(1);
		strcpy(string(buf2), string(buf));
		buf = buf2;
	}
	return buf;
}

cell pp_sys_readdir(void) {
	DIR		*dir;
	struct dirent	*dp;
	cell		n, a, pa;

	dir = opendir(string(parg(1)));
	if (dir == NULL) return sys_error("sys:readdir");
	n = cons(NIL, NIL);
	save(n);
	a = n;
	pa = n;
	while (1) {
		dp = readdir(dir);
		if (dp == NULL)
			break;
		if (	!strcmp(dp->d_name, ".") ||
			!strcmp(dp->d_name, "..")
		)
			continue;
		pa = a;
		assign(car(a), make_string(dp->d_name, strlen(dp->d_name)));
		assign(cdr(a), cons(NIL, NIL));
		a = cdr(a);
	}
	cdr(pa) = NIL;
	if (car(n) == NIL) n = NIL;
	closedir(dir);
	unsave(1);
	return n;
}

cell pp_sys_readlink(void) {
	char	buf[MAXPATHLEN+1];
	int	k;

	k = readlink(string(parg(1)), buf, MAXPATHLEN);
	if (k < 0) return sys_error("sys:readlink");
	buf[k] = 0;
	return make_string(buf, k);
}

cell pp_sys_rename(void) {
	int	r;

	r = rename(string(parg(1)), string(parg(2)));
	if (r < 0) return sys_error("sys:rename");
	return sys_ok();
}

cell pp_sys_rmdir(void) {
	if (rmdir(string(parg(1))) < 0)
		return sys_error("sys:rmdir");
	return sys_ok();
}

cell pp_sys_select(void) {
	cell		p;
	struct timeval	tv, *tvp;
	fd_set		rset, wset;
	char		name[] = "sys:select";
	int		r, k, nfd;
	char		msg[] = "sys:select: expected list of integer, got";

	if (	parg(1) != NIL &&
		(!integer_p(car(parg(1))) ||
		 cdr(parg(1)) == NIL ||
		 !integer_p(cadr(parg(1))) ||
		 cddr(parg(1)) != NIL)
	) {
		error(msg, parg(1));
	}
	FD_ZERO(&rset);
	nfd = 0;
	for (p = parg(2); p != NIL; p = cdr(p)) {
		if (!pair_p(p))
			error("sys:select: improper list", parg(2));
		if (!integer_p(car(p)))
			error(msg, parg(2));
		k = integer_value(name, car(p));
		FD_SET(k, &rset);
		if (k > nfd) nfd = k;
	}
	FD_ZERO(&wset);
	for (p = parg(3); p != NIL; p = cdr(p)) {
		if (!pair_p(p))
			error("sys:select: improper list", parg(3));
		if (!integer_p(car(p)))
			error(msg, parg(3));
		k = integer_value(name, car(p));
		FD_SET(k, &wset);
		if (k > nfd) nfd = k;
	}
	nfd++;
	if (parg(1) == NIL) {
		tvp = NULL;
	}
	else {
		tv.tv_sec = integer_value(name, car(parg(1)));
		tv.tv_usec = integer_value(name, cadr(parg(1)));
		tvp = &tv;
	}
	r = select(nfd, &rset, &wset, NULL, tvp);
	if (r < 0) return sys_error(name);
	return r==0? FALSE: make_integer(r);
}

cell pp_sys_setgid(void) {
	if (setgid(integer_value("sys:setgid", parg(1))) < 0)
		return sys_error("sys:setgid");
	return sys_ok();
}

cell pp_sys_setpgid(void) {
	int	r;

	r = setpgid(0, integer_value("sys:setpgid", parg(1)));
	if (r < 0) return sys_error("sys:setpgid");
	return sys_ok();
}

cell pp_sys_setuid(void) {
	if (setuid(integer_value("sys:setgid", parg(1))) < 0)
		return sys_error("sys:setuid");
	return sys_ok();
}

cell pp_sys_sleep(void) {
	if (sleep(integer_value("sys:sleep", parg(1))))
		return sys_error("sys:sleep");
	return sys_ok();
}

cell sys_stat(int follow) {
	struct stat	sb;
	cell		n, a;

	if ((follow? stat: lstat)(string(parg(1)), &sb) < 0)
		return sys_error(NULL);
	n = cons(NIL, NIL);
	save(n);
	assign(car(n), cons(symbol_ref("name"), parg(1)));
	a = cons(NIL, NIL);
	cdr(n) = a;
	assign(car(a), cons(symbol_ref("size"), NIL));
	assign(cdar(a), make_integer(sb.st_size));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("uid"), NIL));
	assign(cdar(a), make_integer(sb.st_uid));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("gid"), NIL));
	assign(cdar(a), make_integer(sb.st_gid));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("mode"), NIL));
	assign(cdar(a), make_integer(sb.st_mode));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("ctime"), NIL));
	assign(cdar(a), make_integer(sb.st_ctime));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("atime"), NIL));
	assign(cdar(a), make_integer(sb.st_atime));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("mtime"), NIL));
	assign(cdar(a), make_integer(sb.st_mtime));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("dev"), NIL));
	assign(cdar(a), make_integer(sb.st_dev));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("ino"), NIL));
	assign(cdar(a), make_integer(sb.st_ino));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("nlink"), NIL));
	assign(cdar(a), make_integer(sb.st_nlink));
	unsave(1);
	return n;
}

cell pp_sys_stat(void) {
	return sys_stat(1);
}

cell pp_sys_lstat(void) {
	return sys_stat(0);
}

int stat_mode(char *who, int follow) {
	struct stat	st;

	if ((follow? stat: lstat)(string(parg(1)), &st) < 0)
		return sys_error(who);
	return st.st_mode;
}

cell pp_sys_stat_block_dev_p(void) {
	return S_ISBLK(stat_mode("stat-block-dev?", 1))? TRUE: FALSE;
}

cell pp_sys_stat_char_dev_p(void) {
	return S_ISCHR(stat_mode("stat-char-dev?", 1))? TRUE: FALSE;
}

cell pp_sys_stat_directory_p(void) {
	return S_ISDIR(stat_mode("stat-directory?", 1))? TRUE: FALSE;
}

cell pp_sys_stat_pipe_p(void) {
	return S_ISFIFO(stat_mode("stat-pipe?", 1))? TRUE: FALSE;
}

cell pp_sys_stat_regular_p(void) {
	return S_ISREG(stat_mode("stat-regular?", 1))? TRUE: FALSE;
}

cell pp_sys_stat_socket_p(void) {
	return S_ISSOCK(stat_mode("stat-socket?", 1))? TRUE: FALSE;
}

cell pp_sys_lstat_block_dev_p(void) {
	return S_ISBLK(stat_mode("lstat-block-dev?", 0))? TRUE: FALSE;
}

cell pp_sys_lstat_char_dev_p(void) {
	return S_ISCHR(stat_mode("lstat-char-dev?", 0))? TRUE: FALSE;
}

cell pp_sys_lstat_directory_p(void) {
	return S_ISDIR(stat_mode("lstat-directory?", 0))? TRUE: FALSE;
}

cell pp_sys_lstat_pipe_p(void) {
	return S_ISFIFO(stat_mode("lstat-pipe?", 0))? TRUE: FALSE;
}

cell pp_sys_lstat_regular_p(void) {
	return S_ISREG(stat_mode("lstat-regular?", 0))? TRUE: FALSE;
}

cell pp_sys_lstat_socket_p(void) {
	return S_ISSOCK(stat_mode("lstat-socket?", 0))? TRUE: FALSE;
}

cell pp_sys_lstat_symlink_p(void) {
	return S_ISLNK(stat_mode("lstat-symlink?", 0))? TRUE: FALSE;
}

cell pp_sys_strerror(void) {
	char	*s = strerror(integer_value("sys:strerror", parg(1)));

	return make_string(s, strlen(s));
}

cell pp_sys_symlink(void) {
	if (symlink(string(parg(1)), string(parg(2))) < 0)
		return sys_error("sys:symlink");
	return sys_ok();
}

cell pp_sys_system(void) {
	int	r;

	r = system(string(parg(1)));
	if (r < 0 || r > 127) return sys_error("sys:system");
	return make_integer(r);
}

cell pp_sys_gettimeofday(void) {
	struct timeval	t;
	cell		n, m;

	gettimeofday(&t, NULL);
	n = make_integer(t.tv_usec);
	n = cons(n, NIL);
	save(n);
	m = make_integer(t.tv_sec);
	n = cons(m, n);
	unsave(1);
	return n;
}

cell pp_sys_umask(void) {
	int	r;

	if (0 == narg())
		umask(r = umask(0));
	else
		r = umask(integer_value("sys:umask", parg(1)));
	return make_integer(r);
}

cell pp_sys_unlink(void) {
	if (unlink(string(parg(1))) < 0)
		return sys_error("sys:unlink");
	return sys_ok();
}

cell pp_sys_unlock(void) {
	char	p[256], *s;

	s = string(parg(1));
	if (strlen(s) > 248)
		error("sys:unlock: path too long", parg(1));
	sprintf(p, "%s.lock", s);
	rmdir(p);
	return sys_ok();
}

cell pp_sys_usleep(void) {
#if __FreeBSD__ == 7
	int usleep(useconds_t microseconds);
#endif
	if (usleep(integer_value("sys:usleep", parg(1))))
		return sys_error("sys:usleep");
	return sys_ok();
}

cell pp_sys_utimes(void) {
	if (utimes(string(parg(1)), NULL) < 0)
		return sys_error("sys:utimes");
	return sys_ok();
}

cell pp_sys_wait(void) {
	int	r, status;
	cell	n;

	r = wait(&status);
	if (r < 0) return sys_error("sys:wait");
	n = cons(make_integer(r), NIL);
	save(n);
	n = cons(make_integer(WEXITSTATUS(status)), n);
	unsave(1);
	return n;
}

cell pp_sys_waitpid(void) {
	int	r, status;
	char	name[] = "sys:waitpid";

	r = waitpid(integer_value(name, parg(1)), &status, WNOHANG);
	if (r < 0) return sys_error(name);
	return r == 0? FALSE: make_integer(WEXITSTATUS(status));
}

cell pp_sys_write(void) {
	int	r;

	r = write(integer_value("sys:write", parg(1)), string(parg(2)),
		string_len(parg(2))-1);
	if (r < 0) return sys_error("sys:write");
	return make_integer(r);
}

#ifdef NETWORK

cell pp_sys_inet_accept(void) {
	int	r;

	r = accept(integer_value("sys:inet-accept", parg(1)), NULL, 0);
	if (r < 0) return sys_error("sys:inet-accept");
	return make_integer(r);
}

cell pp_sys_inet_connect(void) {
	struct addrinfo	hints, *res, *rp;
	int 		s;
	int		r;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	r = getaddrinfo(string(parg(1)), string(parg(1)), &hints, &res);
	if (r != 0)
		return sys_error("sys:inet-connect/getaddrinfo");
	s = -1;
	for (rp = res; s < 0 && rp; rp = rp->ai_next) {
		s = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
		if (s < 0)
                           continue;
		if (connect(s, rp->ai_addr, rp->ai_addrlen) < 0) {
			close(s);
			s = -1;
			continue;
		}
	}
	if (s < 0)
		return sys_error("sys:inet-connect");
	freeaddrinfo(res);
	return make_integer(s);
}

cell pp_sys_inet_getpeername(void) {
	socklen_t		len;
	struct sockaddr_storage addr;
	char			ip[128];
	int			port, fd;
	cell			n, m;

	fd = integer_value("sys:inet-getpeername", parg(1));
	len = sizeof addr;
	if (getpeername(fd, (struct sockaddr *) &addr, &len) < 0)
		return sys_error(NULL);
	if (addr.ss_family == AF_INET6) {
		struct sockaddr_in6 *s = (struct sockaddr_in6 *) &addr;
		port = ntohs(s->sin6_port);
		inet_ntop(AF_INET6, &s->sin6_addr, ip, sizeof ip);
	}
	else {
		struct sockaddr_in *s = (struct sockaddr_in *) &addr;
		port = ntohs(s->sin_port);
		inet_ntop(AF_INET, &s->sin_addr, ip, sizeof ip);
	}
	n = cons(make_integer(port), NIL);
	save(n);
	m = make_string(ip, strlen(ip));
	n = cons(m, n);
	unsave(1);
	return n;
}

cell pp_sys_inet_listen(void) {
	struct addrinfo	hints, *res, *rp;
	int		s;
	int		r, maxq;
	char		*host;
	struct utsname	u;

	maxq = integer_value("sys:inet-listen", parg(3));
	if (string_p(parg(1))) {
		host = string(parg(1));
	}
	else if (parg(1) == TRUE) {
		r = uname(&u);
		if (r < 0)
			return sys_error("sys:inet-listen/uname");
		host = u.nodename;
	}
	else {
		error("sys:inet-listen: expected string or #t, got",
			parg(1));
		return UNDEFINED; /*LINT*/
	}
	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;
	r = getaddrinfo(host, string(parg(2)), &hints, &res);
	if (r != 0)
		return sys_error("sys:inet-listen/getaddrinfo");
	s = -1;
	for (rp = res; s < 0 && rp; rp = rp->ai_next) {
		s = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
		if (s < 0)
			continue;
		if (bind(s, rp->ai_addr, rp->ai_addrlen) < 0) {
			close(s);
			s = -1;
			continue;
		}
		listen(s, maxq);
	}
	if (s < 0)
		return sys_error("sys:inet-listen");
	freeaddrinfo(res);
	return make_integer(s);
}

#endif /* NETWORK */

S9_PRIM Unix_primitives[] = {
 { "sys:access",           pp_sys_access,           2,  2, { STR,INT,___ } },
 { "sys:catch-errors",     pp_sys_catch_errors,     1,  1, { BOL,___,___ } },
 { "sys:chdir",            pp_sys_chdir,            1,  1, { STR,___,___ } },
 { "sys:close",            pp_sys_close,            1,  1, { INT,___,___ } },
 { "sys:chmod",            pp_sys_chmod,            2,  2, { STR,INT,___ } },
 { "sys:chown",            pp_sys_chown,            3,  3, { STR,INT,INT } },
 { "sys:command-line",     pp_sys_command_line,     0,  0, { ___,___,___ } },
 { "sys:creat",            pp_sys_creat,            2,  2, { STR,INT,___ } },
 { "sys:dup",              pp_sys_dup,              1,  1, { INT,___,___ } },
 { "sys:dup2",             pp_sys_dup2,             2,  2, { INT,INT,___ } },
 { "sys:errno",            pp_sys_errno,            0,  0, { ___,___,___ } },
 { "sys:execv",            pp_sys_execv,            2,  2, { STR,LST,___ } },
 { "sys:exit",             pp_sys_exit,             1,  1, { INT,___,___ } },
 { "sys:fileno",           pp_sys_fileno,           1,  1, { ___,___,___ } },
 { "sys:flush",            pp_sys_flush,            1,  1, { OUP,___,___ } },
 { "sys:fork",             pp_sys_fork,             0,  0, { ___,___,___ } },
 { "sys:getcwd",           pp_sys_getcwd,           0,  0, { ___,___,___ } },
 { "sys:getenv",           pp_sys_getenv,           1,  1, { STR,___,___ } },
 { "sys:getgid",           pp_sys_getgid,           0,  0, { ___,___,___ } },
 { "sys:getgrnam",         pp_sys_getgrnam,         1,  1, { STR,___,___ } },
 { "sys:getgrgid",         pp_sys_getgrgid,         1,  1, { INT,___,___ } },
 { "sys:getpgid",          pp_sys_getpgid,          0,  0, { ___,___,___ } },
 { "sys:getpid",           pp_sys_getpid,           0,  0, { ___,___,___ } },
 { "sys:getpwent",         pp_sys_getpwent,         0,  0, { ___,___,___ } },
 { "sys:getpwnam",         pp_sys_getpwnam,         1,  1, { STR,___,___ } },
 { "sys:getpwuid",         pp_sys_getpwuid,         1,  1, { INT,___,___ } },
 { "sys:gettimeofday",     pp_sys_gettimeofday,     0,  0, { ___,___,___ } },
 { "sys:getuid",           pp_sys_getuid,           0,  0, { ___,___,___ } },
 { "sys:kill",             pp_sys_kill,             2,  2, { INT,INT,___ } },
 { "sys:link",             pp_sys_link,             2,  2, { STR,STR,___ } },
 { "sys:lock",             pp_sys_lock,             1,  1, { STR,___,___ } },
 { "sys:lseek",            pp_sys_lseek,            3,  3, { INT,INT,INT } },
 { "sys:lstat",            pp_sys_lstat,            1,  1, { STR,___,___ } },
 { "sys:lstat-block-dev?", pp_sys_lstat_block_dev_p,1,  1, { STR,___,___ } },
 { "sys:lstat-char-dev?",  pp_sys_lstat_char_dev_p, 1,  1, { STR,___,___ } },
 { "sys:lstat-directory?", pp_sys_lstat_directory_p,1,  1, { STR,___,___ } },
 { "sys:lstat-pipe?",      pp_sys_lstat_pipe_p,     1,  1, { STR,___,___ } },
 { "sys:lstat-regular?",   pp_sys_lstat_regular_p,  1,  1, { STR,___,___ } },
 { "sys:lstat-socket?",    pp_sys_lstat_socket_p,   1,  1, { STR,___,___ } },
 { "sys:lstat-symlink?",   pp_sys_lstat_symlink_p,  1,  1, { STR,___,___ } },
 { "sys:get-magic-value",  pp_sys_get_magic_value,  1,  1, { STR,___,___ } },
 { "sys:make-input-port",  pp_sys_make_input_port,  1,  1, { INT,___,___ } },
 { "sys:make-output-port", pp_sys_make_output_port, 1,  1, { INT,___,___ } },
 { "sys:mkdir",            pp_sys_mkdir,            2,  2, { STR,INT,___ } },
 { "sys:open",             pp_sys_open,             2,  2, { STR,INT,___ } },
 { "sys:pipe",             pp_sys_pipe,             0,  0, { ___,___,___ } },
 { "sys:read",             pp_sys_read,             2,  2, { INT,INT,___ } },
 { "sys:readdir",          pp_sys_readdir,          1,  1, { STR,___,___ } },
 { "sys:readlink",         pp_sys_readlink,         1,  1, { STR,___,___ } },
 { "sys:rename",           pp_sys_rename,           2,  2, { STR,STR,___ } },
 { "sys:rmdir",            pp_sys_rmdir,            1,  1, { STR,___,___ } },
 { "sys:setgid",           pp_sys_setgid,           1,  1, { INT,___,___ } },
 { "sys:select",           pp_sys_select,           3,  3, { LST,LST,LST } },
 { "sys:setpgid",          pp_sys_setpgid,          1,  1, { INT,___,___ } },
 { "sys:setuid",           pp_sys_setuid,           1,  1, { INT,___,___ } },
 { "sys:sleep",            pp_sys_sleep,            1,  1, { INT,___,___ } },
 { "sys:stat",             pp_sys_stat,             1,  1, { STR,___,___ } },
 { "sys:stat-block-dev?",  pp_sys_stat_block_dev_p, 1,  1, { STR,___,___ } },
 { "sys:stat-char-dev?",   pp_sys_stat_char_dev_p,  1,  1, { STR,___,___ } },
 { "sys:stat-directory?",  pp_sys_stat_directory_p, 1,  1, { STR,___,___ } },
 { "sys:stat-pipe?",       pp_sys_stat_pipe_p,      1,  1, { STR,___,___ } },
 { "sys:stat-regular?",    pp_sys_stat_regular_p,   1,  1, { STR,___,___ } },
 { "sys:stat-socket?",     pp_sys_stat_socket_p,    1,  1, { STR,___,___ } },
 { "sys:strerror",         pp_sys_strerror,         1,  1, { INT,___,___ } },
 { "sys:symlink",          pp_sys_symlink,          2,  2, { STR,STR,___ } },
 { "sys:system",           pp_sys_system,           1,  1, { STR,___,___ } },
 { "sys:umask",            pp_sys_umask,            0,  1, { INT,___,___ } },
 { "sys:unlink",           pp_sys_unlink,           1,  1, { STR,___,___ } },
 { "sys:unlock",           pp_sys_unlock,           1,  1, { STR,___,___ } },
 { "sys:usleep",           pp_sys_usleep,           1,  1, { INT,___,___ } },
 { "sys:utimes",           pp_sys_utimes,           1,  1, { STR,___,___ } },
 { "sys:wait",             pp_sys_wait,             0,  0, { ___,___,___ } },
 { "sys:waitpid",          pp_sys_waitpid,          1,  1, { INT,___,___ } },
 { "sys:write",            pp_sys_write,            2,  2, { INT,STR,___ } },
#ifdef NETWORK
 { "sys:inet-accept",      pp_sys_inet_accept,      1,  1, { INT,___,___ } },
 { "sys:inet-connect",     pp_sys_inet_connect,     2,  2, { STR,STR,___ } },
 { "sys:inet-getpeername", pp_sys_inet_getpeername, 1,  1, { INT,___,___ } },
 { "sys:inet-listen",      pp_sys_inet_listen,      3,  3, { ___,STR,INT } },
#endif /* NETWORK */
 { NULL }
};

void sys_init(void) {
	signal(SIGPIPE, SIG_IGN);
	add_primitives("sys-unix", Unix_primitives);
#ifdef NETWORK
	add_primitives("net-unix", NULL);
#endif /* NETWORK */
}
