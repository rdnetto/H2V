f2 a b c = z where
    z = f2c c
    f2c x = f2a a + f2b d - x
    f2a x = x + a + d
    f2b x = x + b
    d = a + b
