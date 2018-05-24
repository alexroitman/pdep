esbisiesto:: Integer->Bool
esbisiesto año =(((mod año 4 == 0) && (mod año 100 /= 0)) ||(mod año 400 == 0))
diasBisiesto:: Bool->Integer
diasBisiesto True=366
diasBisiesto False=365
añoDiasBisiesto año=(diasBisiesto.esbisiesto) año