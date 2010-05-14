gcc -c j_acos.c
gcc -c j_erf.c
gcc -c j_acsh.c
gcc -c j_acot.c
gcc -c j_acth.c
gcc -c j_asin.c
gcc -c j_asnh.c
gcc -c j_atan.c
gcc -c j_atnh.c
gcc -c j_cos.c 
gcc -c j_cosh.c
gcc -c j_cot.c
gcc -c j_coth.c
gcc -c j_exp.c
gcc -c j_ex10.c
gcc -c j_exp2.c
gcc -c j_expm.c
gcc -c j_log.c
gcc -c j_lg10.c
gcc -c j_lg1p.c
gcc -c j_log2.c
gcc -c j_sin.c
gcc -c j_sinh.c
gcc -c j_sqr.c
gcc -c j_sqrt.c
gcc -c j_tan.c
gcc -c j_tanh.c
gcc -c q_acos.c
gcc -c q_erf.c
gcc -c q_acsh.c
gcc -c q_acot.c
gcc -c q_acth.c
gcc -c q_asin.c
gcc -c q_asnh.c
gcc -c q_atan.c
gcc -c q_atnh.c
gcc -c q_atn1.c
gcc -c q_cos.c
gcc -c q_cos1.c
gcc -c q_cosh.c
gcc -c q_cot.c
gcc -c q_coth.c
gcc -c q_ep1.c
gcc -c q_epm1.c
gcc -c q_exp.c
gcc -c q_ex10.c
gcc -c q_exp2.c
gcc -c q_expm.c
gcc -c q_glbl.c
gcc -c q_log.c
gcc -c q_log1.c
gcc -c q_lg10.c
gcc -c q_log2.c
gcc -c q_pred.c
gcc -c q_rtrg.c
gcc -c q_sin.c
gcc -c q_sin1.c
gcc -c q_sinh.c
gcc -c q_sqr.c
gcc -c q_sqrt.c
gcc -c q_succ.c
gcc -c q_tan.c
gcc -c q_tanh.c
gcc -c q_cth1.c
gcc -c q_scan.c
gcc -c q_prnt.c
gcc -c q_ari.c
gcc -c q_errm.c
gcc -c q_comp.c 
ar -r fi_lib.a j_acos.o 
ar -r fi_lib.a j_erf.o 
ar -r fi_lib.a j_acsh.o
ar -r fi_lib.a j_acot.o
ar -r fi_lib.a j_acth.o
ar -r fi_lib.a j_asin.o
ar -r fi_lib.a j_asnh.o
ar -r fi_lib.a j_atan.o
ar -r fi_lib.a j_atnh.o
ar -r fi_lib.a j_cos.o
ar -r fi_lib.a j_cosh.o
ar -r fi_lib.a j_cot.o
ar -r fi_lib.a j_coth.o
ar -r fi_lib.a j_exp.o
ar -r fi_lib.a j_ex10.o
ar -r fi_lib.a j_exp2.o
ar -r fi_lib.a j_expm.o
ar -r fi_lib.a j_log.o
ar -r fi_lib.a j_lg10.o
ar -r fi_lib.a j_lg1p.o
ar -r fi_lib.a j_log2.o
ar -r fi_lib.a j_sin.o
ar -r fi_lib.a j_sinh.o
ar -r fi_lib.a j_sqr.o
ar -r fi_lib.a j_sqrt.o
ar -r fi_lib.a j_tan.o
ar -r fi_lib.a j_tanh.o
ar -r fi_lib.a q_erf.o
ar -r fi_lib.a q_acos.o
ar -r fi_lib.a q_acsh.o
ar -r fi_lib.a q_acot.o
ar -r fi_lib.a q_acth.o
ar -r fi_lib.a q_asin.o
ar -r fi_lib.a q_asnh.o
ar -r fi_lib.a q_atan.o
ar -r fi_lib.a q_atnh.o
ar -r fi_lib.a q_atn1.o
ar -r fi_lib.a q_cos.o
ar -r fi_lib.a q_cos1.o
ar -r fi_lib.a q_cosh.o
ar -r fi_lib.a q_cot.o
ar -r fi_lib.a q_coth.o
ar -r fi_lib.a q_ep1.o
ar -r fi_lib.a q_epm1.o
ar -r fi_lib.a q_exp.o
ar -r fi_lib.a q_ex10.o
ar -r fi_lib.a q_exp2.o
ar -r fi_lib.a q_expm.o
ar -r fi_lib.a q_glbl.o
ar -r fi_lib.a q_log.o
ar -r fi_lib.a q_log1.o
ar -r fi_lib.a q_lg10.o
ar -r fi_lib.a q_log2.o
ar -r fi_lib.a q_rtrg.o
ar -r fi_lib.a q_sin.o
ar -r fi_lib.a q_sin1.o
ar -r fi_lib.a q_sinh.o
ar -r fi_lib.a q_sqr.o
ar -r fi_lib.a q_sqrt.o
ar -r fi_lib.a q_tan.o
ar -r fi_lib.a q_tanh.o
ar -r fi_lib.a q_cth1.o
ar -r fi_lib.a q_ari.o
ar -r fi_lib.a q_scan.o
ar -r fi_lib.a q_prnt.o
ar -r fi_lib.a q_errm.o
ar -r fi_lib.a q_comp.o 
ar -r fi_lib.a q_pred.o
ar -r fi_lib.a q_succ.o
gcc -o test\fi_test.exe -I. test\fi_test.c fi_lib.a -lm
gcc -o example\hornerc.exe -I. example\hornerc.c fi_lib.a -lm
gcc -o example\comp_sin.exe -I. example\comp_sin.c fi_lib.a -lm
gcc -o example\comp_exp.exe -I. example\comp_exp.c fi_lib.a -lm
g++ -o example\hornercpp.exe -I. example\hornercpp.c fi_lib.a -lm
g++ -o example\bisection.exe -I. example\bisection.c fi_lib.a -lm
g++ -o example\xinewton.exe -I. example\xinewton.c fi_lib.a -lm

