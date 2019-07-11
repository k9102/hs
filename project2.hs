l = [("Kim", 60), ("Park",80), ("Choi", 70), ("Lee", 90), ("Jung", 85)]

fl = [x | x<-l,snd x > 80]
f2 = filter (\x->snd x>80) l
f3 = filter ((>80).snd) l