5 CLS : PRINT : PRINT
10 PRINT "    two-layer system": PRINT
15 PRINT "    input a parameters of the system"
20 INPUT "    length of wawes"; L
21 PRINT "    support:"
22 INPUT "N3"; N3: INPUT "X3"; X3
23 PRINT "     layer:"
24 INPUT "N2="; N2: INPUT "X2="; X2: INPUT "D2="; D2: INPUT "N1=";
N1: INPUT "D1="; D1
25 PI = 3.141592654#: S = 4 * PI/L
30 A2 = N2 * N2 - X2 * X2: A3 = N3 * N3 - X3 * X3:B2 = 2 * N2 * X2:
33 WHILE Q >= 1
34 CLS
35 PRINT : PRINT "    specification task:"
41 COLOR 7, 0: PRINT "    calculation of"; : COLOR 15, 0: PRINT "e";:
COLOR 7, 0: PRINT "lipsometric angeles"
42 COLOR 7, 0: PRINT "    calculation of"; : COLOR 15, 0: PRINT "m";:
COLOR 7, 0: PRINT "ain angele"
43 COLOR 7, 0: PRINT "    calculation of"; : COLOR 15, 0: PRINT "p";:
COLOR 7, 0: PRINT "arameters of surface"
44 COLOR 7, 0: PRINT "    calculation of"; : COLOR 15, 0: PRINT "a";:
COLOR 7, 0: PRINT "rameters of support"
50 Q$ = INPUT$(1): Q = INSTR("empa", Q$)
51 IF Q = 0 GOTO 50
52 CLS
53 ON Q GOTO 70, 140, 300, 415
54 WEND
62 REM ******************** calculation of elipsometric angeles **************
70 PRINT "    input parameters of surface"
75 INPUT "N1="; N1
80 INPUT "D1="; D1
85 PRINT "    input thickness of a layer"
90 INPUT "D2="; D2
95 PRINT "input first F1, last F2 angle of incidence and step F0"
100 INPUT "F1="; F1: INPUT "F2="; F2: INPUT "F0="; F0
105 FOR F = F1 TO F2 STEP F0
110 GOSUB 500: k = COS(o): o = o * 180/PI: PRINT F, T, k, o
115 NEXT
120 PRINT "    go on this task? (y/n)"
121 A$ = " ": A$ = INPUT$(1): IF A$ <> "n" THEN 121
122 IF A$ = "y" THEN 70: IF A$ <> "n" THEN 120
123 PRINT "    exit to main menu? (y/n)"
124 A$ = ""; A$ = INPUT$(1): IF A$ = " " THEN 124
125 IF A$ = "y" THEN 5: IF A$ <> "n" THEN 123
130 GOTO 680
139 REM ************* calculation of main angle ************
140 PRINT "    input parameters of surface"
145 INPUT "N1=": N1
150 INPUT "D1=": D1
155 PRINT "    input thickness of a layer": INPUT "D2="; D2
160 INPUT "    MAX angel of incidence="; W
165 INPUT "    MIN angel of incidence="; U
170 F = W
175 GOUSB 500: k = COS(o): F = U
180 GOSUB 500: IF k * COS(o) < 0 THEN 190
185 GOTO 260
190 F = (W + U)/2: I = F
195 GOSUB 500; k = COS(0): IF ABS(k) <= .001 THEN 220
200 F = W
205 GOSUB 500: IF k * COS(o) < 0 THEN 215
210 W = I: GOTO 190
215 U = I: FOTO 190
220 PRINT D1, F, T, k
224 PRINT "    go on? (y/n)"
225 A$ = " ": A$ = INPUT$(1): IF A$ = " " THEN 225
226 IF A$ = "y" THEN 140: IF A$ <> "n" THEN 224
227 PRINT "    exit to main menu? (y/n)"
228 A$ = " ": A$ = INPUT$(1): IF A$ = " " THEN 228
229 IF A$ = "y" THEN 5: IF A$ <> "n" THEN 227
240 GOTO 680
260 PRINT "   No solution:", "cos(w)="; k, "cos(u)="; COS(o)
262 PRINT "    go on this task with new parameters of surface? <y/n>"
263 A$=" ": A$ = INPUT$(1): IF A$ = " " THEN 263
264 IF A$ = "y" THEN 140: IF A$ <> "n" THEN 262
266 PRINT "    go on this task with old parameters of surface? (y/n)"
267 A$ = " ": A$ = INPUT$(1): IF A$ = " " THEN 263
268 IF A$ = " ": A$ = INPUT$(1): IF A$ = " " THEN 262
275 PRINT "    exit to main menu? (y/n)"
276 A$ = INPUT$(1): IF A$ = " " THEN 266
277 IF A$ = "y" THEN 5: IF A$ <> "n" THEN 275
280 GOTO 680
299 REM ****************** calculation of a parameters of surface ****************
300 CLS : PRINT "    input result of measurements"
305 INPUT "F="; F: INPUT "TAN="; RO: INPUT "cos="; DEL
310 PRINT "    input approximate root"
315 INPUT "N1="; N1: INPUT "D1="; D1: V = 0
320 GOSUB 500: W = T - RO: U = COS(o) - DEL
325 IF ABS(U) <= .0001 AND ABS(W) <= .0001 THEN 395
330 N1 = N1 - .01
335 GOSUB 500: H = T: k = COS(o): N1 = N1 + .02
340 GOSUB 500: H = (T - H)/.02:  D1 = D1 -.1
342 N1 = N1 - .01
345 GOSUB 500: I = T: J = COS(o): D1 = D2 + .2
350 GOSUB 500: I = (T -I)/.2: J = (COS(o) - J)/.2
355 A = H * J - I * k: B = U * I - W * J: C = k * W - H * U
360 N1 = N1 + B/A: D1 = D1 + C / A - .1: V = V + 1
365 IF N1 < 1 THEN 385
370 IF D1 < 0 THEN 385
375 IF V = 50 THEN 385
380 GOTO 320
385 PRINT "No solution:", "N1="; N1, "D1="; D1, "number of iterate="; V
390 GOTO 402
395 PRINT F,RO,DEL
400 PRINT "N1="; N1, "D1="; D1, W, U, V
402 PRINT "    go on this task? (y/n)";
403 A$ = " ":A$ = INPUT$(1); IF A$ = " " THEN 403
405 IF A$ = "y" THEN 300: IF A$ <> "n" THEN 402
410 PRINT : PRINT "    exit to main menu? (y/n)"
411 A$ = " ":A$ = INPUT$(1): IF A$ = " " THEN 411
412 IF A$ = "y" THEN 5: IF A$ <> "n" THEN 410
413 GOTO 680
414 REM ******************** calculate parameters of support ********************
415 PRINT "     input result of measurements"
418 INPUT "F="; F: INPUT "TAN="; RO: INPUT "cos="; DEL:
420 PRINT "     input approximate root"
423 INPUT "N3="; N3: INPUT "X3="; X3: V = 0
425 GOSUB 500: W = T - RO: U = COS(o) - DEL
427 IF ABS(U) <= .0001 AND ABS(W) <= .0001 THEN 480
430 N3 = N3 - .01
435 GOSUB 500: H = T: k = COS(o): N3 = N3 + .02
440 GOSUB 500: H = (T - H)/.02: k = (COS(o) - k)/.02: X3 = X3 - .01
442 N3 = N3 - .01
445 GOSUB 500: I = T: J = COS(o): X3 = X3 + .02
447 GOSUB 500: I = (T -I)/ .02: J = (COS(o) -J)/.02
450 A = H * J - I * k: B = U * I -W * J: C = K * W - H * U
451 IF A = 0 THEN 484
455 N3  = N3 + B / A: X3 = X3 + C / A - .01: V = V + 1 
465 If V = 150  THEN 470
467 GOTO 425
470 PRINT "    No solution:", "N3="; N3, "X3="; X3, "W="; W, "U="; U,
"V="; V
475 GOTO 485
480 PRINT F,RO,DEL
483 PRINT N3, X3, W, U, V: GOTO 485
484 PRINT "V="; V, "A="; A, "N3="; N3, "X3="; X3
485 PRINT "     go on this task? (y/n)"
486 A$ = " ":A$ = INPUT$(1): IF A$ = " " THEN 486
487 IF A$ = "y" THEN 415: IF A$ <> "n" THEN 485
490 PRINT "     exit to main menu? (y/n)"
491 A$ = " ": A$ = INPUT$(1): IF A$ = " " THEN 491
492 IF A$ = "y" THEN 5: IF A$ <> "n" THEN 490
493 GOTO 680
499 REM **************** ellipsometric function *********************
500 F = F * PI / 180: SI = SIN(F) * SIN(F): A2 = N2 * N2 - X2 * X2: A3 = N3
502 M = SQR(A * A + B2 * B2)
505 C = SQR((M + A) / 2): D = -SQR((M - A) / 2): A = A3 - SI:
507 M = SQR(A * A + B3 * B3)
510 E = SQR((M + A) / 2): G = -SQR((M - A) / 2): P = S * D2 * C;
512 Q = S * D2 * D
515 K1 = SQR(N1 * N1 - SI): Z1 = S * D1 * K1: X = N1 * N1 * COS(F):
517 R1 = (X - K1)/(X + K1)
520 X = A2 * K1 / (N1 * N1): M = X - C: A = X + C:
522 X = B2 * K1 / N1 /N1: n = -(X + D): B = D -X
525 GOSUB 650: A = M: B = n
530 GOSUB 655: R2 = T: 02 = o
535 X = A3 * C + B3 * D: y = A2 * E + B2 * G: A = X + y: M = X -y
540 X = A3 * D - B3 * C: y = A2 * G - B2 * E: B = X +y: n = X - y
545 GOSUB 650: A = M: B = n
550 GOSUB 655: R3 = T: 03 = o
555 GOSUB 610: RP = T: OP = o
560 R1 = (COS(F) - K1) / (COS(F) + K1): A = K1 + C: B = D
570 M = K1 - C: n = -D
575 GOSUB 650: A = M: B = n
580 A = C + E: B = D + G: M = C - E: n = D - G
585 GOSUB 650: A  = M: B = n
590 GOSUB 655: R3 = T: O3 = o
595 GOSUB 610: RS = T: OS = o
600 T = RP / RS: o = OP - OS: F = F * 180 / PI
605 RETURN
610 R = R2 * R3 * EXP(Q): Y1 = O2 + O3 - P: Y2 = O3 - Z1 -P: Y3 = O2 -
Z1
615 X = 1 + R * cos(Y1): y = R2 * COS(Y) / R2
620 M = R1 * X +y: A = R1 * y + X
625 X = R * SIN(Y1): y = R2 * SIN(Y3) + R * SIN(Y2)/R2
630 n = R1 * X + y: B = R1 * y + X
635 GOSUB 650: A = M: B = n
640 GOSUB 655
645 RETURN
650 T = 1: o = O
655 R = SQR(A * A + B * B): Z = ATN(B / A)
660 IF A < 0 THEN Z = Z - PI
665 T = R / T: o = Z -o
670 RETURN
680 END