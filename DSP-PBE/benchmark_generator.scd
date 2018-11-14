(
b = Buffer.read(s,"/home/mark/Github/DSP-PBE/DSP-PBE/Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav");
r = Buffer.alloc(s, 44100 * 1.0, 2); // a 1 second, 2 channel Buffer
r.sampleRate
)

(
SynthDef(\myLPF, {| bufPlay , bufRecord |
	var lpfSound;
    lpfSound = LPF.ar( PlayBuf.ar(1, bufPlay, BufRateScale.kr(bufPlay)),2000);
	RecordBuf.ar([lpfSound,lpfSound], bufRecord, doneAction: Done.freeSelf, loop: 0)
}).play(s, [\bufPlay, b, \bufRecord, r]);
)

(
SynthDef(\myLPF, {| bufPlay , bufRecord |
	var lpfSound, ringz, ps;
	ringz = 0.1 * Ringz.ar(PlayBuf.ar(1, bufPlay, BufRateScale.kr(bufPlay)),2000,0.2);

	ps = FreqShift.ar(PlayBuf.ar(1, bufPlay, BufRateScale.kr(bufPlay)), -1000);

	lpfSound = LPF.ar( ps, 1000);
	RecordBuf.ar([lpfSound+ringz,lpfSound+ringz], bufRecord, doneAction: Done.freeSelf, loop: 0)
}).play(s, [\bufPlay, b, \bufRecord, r]);
)

r.plot;

(
r.write(path: "/home/mark/Github/DSP-PBE/DSP-PBE/Sounds/SynthesisBenchmarks/Constructed/cartoon010-sc-lpf2000.wav", headerFormat: 'wav', sampleFormat: 'int16');
r.sampleRate
)

(
b.close; b.free;
r.close; r.free;
)

(
SynthDef(\help_RecordBuf_playback, { arg out = 0, bufnum = 0;
    var playbuf;
    playbuf = PlayBuf.ar(1,bufnum);
    FreeSelfWhenDone.kr(playbuf); // frees the synth when the PlayBuf is finished
    Out.ar(out, playbuf);
}).play(s, [\out, 0, \bufnum, r]);
)

s.options.sampleRate = 44100
s.reboot
s.quit