#include <stdio.h>
#include <string.h>
#include <termios.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <locale.h>
#include "scheme.h"
#include "petite_boot.h"
#include "scheme_boot.h"
#include "main_boot.h"

int get_echo() { return ECHO; }
int get_isig() { return ISIG; }
int get_icanon() { return ICANON; }
int get_ixon() { return IXON; }
int get_iexten() { return IEXTEN; }
int get_icrnl() { return ICRNL; }
int get_tcsaflush() { return TCSAFLUSH; }
unsigned long get_tiocgwinsz() { return TIOCGWINSZ; }
int get_lc_all() { return LC_ALL; }
int get_sigwinch() {
    #ifdef SIGWINCH
        return SIGWINCH;
    #else
        return 28; 
    #endif
}

int main(int argc, const char *argv[]) {
    Sscheme_init(NULL);

    Sregister_boot_file_bytes("petite.boot", (void *)petite_boot, (iptr)petite_boot_len);
    Sregister_boot_file_bytes("scheme.boot", (void *)scheme_boot, (iptr)scheme_boot_len);
    Sregister_boot_file_bytes("main.boot", (void *)main_boot, (iptr)main_boot_len);

    Sbuild_heap(NULL, NULL);

    Sscheme_start(argc, argv); 

    Sscheme_deinit();
    return 0;
}