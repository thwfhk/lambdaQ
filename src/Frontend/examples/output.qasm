openqasm 2.0;
qreg r0[1];
qreg r1[1];
H r0;
H r1;
CX r0, r1;
H r0;
H r1;
