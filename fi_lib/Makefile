# /*********************************************************************/
# /*       fi_lib  --- A fast interval library (Makefile)              */
# /*       (For copyright and info`s look at file "fi_lib.h")          */
# /*********************************************************************/

#----------- Start checking here ----------------------------------------
CC = gcc#          !!! name of your C compiler   (e.g. gcc, cc, xlc, ...) 
CPP = g++#         !!! name of your C++ compiler (e.g. g++, CC, xlC, ...)
CFLAGS = #         !!! options for your C/C++ Compiler (for HP: -Aa) 
COPT = #           !!! optimization options (e.g. -O2, -O, ...)
#                  !!! Caution! These options can cause problems!
#
#----------- Stop checking here -----------------------------------------

# Default and clean up rules
#---------------------------
default: all

# Delete all *.o *.a and executable files
#----------------------------------------
clean:	
	rm -f *.a *.o

LIB = fi_lib.a

LIBRARIES =  $(LIB) -lm

OBJECTS = j_acos.o j_acsh.o j_acot.o j_acth.o j_asin.o j_asnh.o j_atan.o \
          j_atnh.o j_cos.o j_cosh.o j_cot.o j_coth.o j_exp.o j_ex10.o j_exp2.o \
          j_expm.o j_log.o j_lg10.o j_lg1p.o j_log2.o j_sin.o j_sinh.o j_sqr.o \
          j_sqrt.o j_tan.o j_tanh.o j_erf.o q_acos.o q_acsh.o q_acot.o q_acth.o \
          q_asin.o q_asnh.o q_atan.o q_atnh.o q_atn1.o q_cos.o q_cos1.o \
          q_cosh.o q_cot.o q_coth.o q_ep1.o q_epm1.o q_exp.o q_ex10.o q_exp2.o \
          q_expm.o q_glbl.o q_log.o q_log1.o q_lg10.o q_log2.o q_pred.o \
          q_rtrg.o q_sin.o q_sin1.o q_sinh.o q_sqr.o q_sqrt.o q_succ.o q_tan.o \
          q_tanh.o q_cth1.o q_erf.o q_ari.o q_errm.o

$(LIB): $(OBJECTS)
	ar rv $@ $(OBJECTS)

%.o: %.c fi_lib.h
	$(CC) -c $(CFLAGS) $(COPT) $<


all: $(LIB)

