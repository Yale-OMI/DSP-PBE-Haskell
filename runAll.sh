echo "Should find lpf 800"
time dist/build/musicSynth/musicSynth Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav Sounds/SynthesisBenchmarks/Constructed/cartoon010-lpf800.wav > lpf800.txt &&
echo "Should find lpf 5000"
time dist/build/musicSynth/musicSynth Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav Sounds/SynthesisBenchmarks/Constructed/cartoon010-lpf5000.wav > lpf5000.txt &&
echo "Should find hpf 1500"
time dist/build/musicSynth/musicSynth Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav Sounds/SynthesisBenchmarks/Constructed/cartoon010-hpf1500.wav > hpf1500.txt &&
echo "Should find lpf 2000"
time dist/build/musicSynth/musicSynth Sounds/SynthesisBenchmarks/Constructed/BTS-DNA-short.wav Sounds/SynthesisBenchmarks/Constructed/BTS-DNA-short-lpf2000.wav > lpf2000.txt && 
echo "Should find hpf 3500"
time dist/build/musicSynth/musicSynth Sounds/SynthesisBenchmarks/Constructed/Holst.wav Sounds/SynthesisBenchmarks/Constructed/Holst-hpf3500.wav > hpf3500.txt
