ROOT = ..
include ${ROOT}/Makefile.inc

# Not compiling: ps2_2 ps2_3
DIRS = ps1_1 ps1_3a ps1_3b ps1_3c ps2_2a ps2_2b ps2_3a \
		ps2_5 ps3_3 ps3_4 ps3_5 ps4_2 ps4_1 ps5_5

all:
	@for d in ${DIRS}; do                          \
		cd $$d;                                \
		for f in *.tex; do                     \
			pdflatex $$f  1>/dev/null;     \
		done;                                  \
		${RM} ${TEMPFILES};                    \
		cd ..;                                 \
	done

clean:
	@for d in ${DIRS}; do                          \
		cd $$d;                                \
		${RM} ${TEMPFILES} ${PDFFILES};        \
		cd ..;                                 \
	done
