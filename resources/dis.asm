200 VB = 8
202 I = &_sprite0
    do {
204     draw(x=V9, y=VA, h=8)
206     I += VB
208     V9 += 8
20A     if(V9 == 64)
20C         VA += 8
20E     if(V9 == 64)
210         V9 = 0
212 while(VA != 32)
214 }
216 VC = rand() & 0xFF
218 VD = 0
21A VE = 0
21C I = &_sprite1
21E draw(x=VC, y=VD, h=8)
    loop {
220     V9 = rand() & 0xFF
222     VA = 0
224     I = &_sprite2
226     draw(x=V9, y=VA, h=7)
        do {
228         I = &_sprite3
22A         draw(x=V9, y=VA, h=8)
22C         VA += 1
22E         call _func0
230         draw(x=VC, y=VD, h=8)
232         VF = 10
234         delay = VE
            do {
236             VF = delay
238         while(VF != 0)
23A         }
23C     while(VA != 25)
23E     }
240     VF = 6
242     if(!key[VF])
244         continue
246     I = &_sprite4
248     V0 = 3
24A     V1 = 7
24C     draw(x=V0, y=V1, h=0)
24E }
_func0:
if(VE != 1) {
    I = &_sprite6
    VE = 1
}
else {
    I = &_sprite5
    VC += -1
    VE = 0
}
sne ve, 1
jmp 0x324
250 if(VE == 1)
252     goto _func1
254 I = &_sprite5
256 VE = 1
258 return
_func1:
25A I = &_sprite6
25C VC += 255
25E VE = 0
260 return
_sprite0:
262 0x0000
264 0x0000
266 0x0000
268 0x0000
26A 0x0000
26C 0x0000
26E 0x0000
270 0x0000
272 0x0000
274 0x0000
276 0x0000
278 0x0000
27A 0x0000
27C 0x0000
27E 0x0000
280 0x0000
282 0x0000
284 0x0000
286 0x0000
288 0x0000
28A 0x0000
28C 0x0000
28E 0x0000
290 0x0000
292 0x0000
294 0x0000
296 0x0000
298 0x0000
29A 0x0000
29C 0x0000
29E 0x0000
2A0 0x0000
2A2 0x0000
2A4 0x0103
2A6 0x0307
2A8 0x0707
2AA VE += 255
2AC 0xFFE7
2AE V3 = rand() & 0x83
2B0 0x0307
2B2 0x0000
2B4 V0 = V8
2B6 V0 = V8
2B8 V0 = V8
2BA 0x0000
2BC 0x0000
2BE 0x0000
2C0 V0 = 96
2C2 0x0000
2C4 0x0000
2C6 0x0000
2C8 0x0000
2CA 0x0000
2CC 0xE070
2CE V0 += 96
2D0 0xE0C0
2D2 0x0000
2D4 0x0000
2D6 0x0000
2D8 0x0000
2DA 0x0000
2DC 0x0000
2DE 0x0000
2E0 0x0000
2E2 0x0707
2E4 0x0703
2E6 0x0100
2E8 call *0x030
2EA 0x070F
2EC if(VF == VF)
2EE 0xFCF8
2F0 0x0000
2F2 if(VD != 125)
2F4 V0 = 96
2F6 V9 += 57
2F8 0x0000
2FA 0xF7F7
2FC VC = rand() & 0xCC
2FE VF = rand() & 0x87
300 0x0060
302 V0 |= VC
304 draw(x=VD, y=VC, h=1)
306 V3 = rand() & 0x83
308 0x0000
30A 0xF1FB
30C if(VB == V9)
30E 0xFBF1
310 0x0000
312 0xE2B3
314 V7 = delay
316 0xE6EE
318 0x0000
31A V0 = rand() & 0xE0
31C call *0x000
31E 0x0000
320 call *0x030
322 call *0xC23
324 I = 0x068
326 goto *0x44B
328 call *0x010
32A call *0x254
32C I = 4A5
32E call *0x665
330 V8 = rand() & 0x00
332 0x0041
334 I = 232
336 I = E51
338 V9 += V8
33A V0 = rand() & 0x40
33C if(V1 == 70)
33E if(V9 == 234)
340 goto *0xA89
342 0x0001
344 V0 = rand() & 0x40
346 if(V7 == 42)
348 goto *0x209
34A V8 += 150
34C if(V9 == 36)
34E call *0x422
350 call *0x244
352 0x0007
354 goto *(V0 + 2112)
356 if(V3 == 36)
358 call *0x819
35A 0x5094
35C goto *0x6A5
35E if(VF == 242)
360 if(V5 == V2)
_sprite2:
362 0x0103
364 0x0509
366 if(V2 != 198)
368 if(V8 != 0)
_sprite3:
36A 0x0102
36C 0x060C
36E if(VB != 244)
370 0xFE38
_sprite1:
372 0x0000
374 0x0036
376 VF += 107
378 if(V5 != V0)
_sprite5:
37A 0x0028
37C 0xEED8
37E 0x037B
380 VD += 0
_sprite6:
382 0x0014
384 V7 += 65
386 if(V1 == 99)
388 if(V1 == 0)
_sprite4:
38A 0x0000
38C 0x0000
38E 0x0000
390 0x0000
392 0x08C0
394 0x0FE0
396 0x01E0
398 goto *0x798
39A goto *0xDD0
39C 0x08C0
39E goto *0x8E0
3A0 0x08C0
3A2 0x0100
3A4 0x0000
3A6 0x0000
3A8 0x0000