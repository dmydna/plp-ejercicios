### Ejercicio 1


`1C-2025-1R`:s

![](/src/ej1_2025c1_r.png)


```hs
data LineaProd = Materiales [String]
                | Agregar String LineaProd
                | Unir LineaProd LineaProd


foldProd :: ([String]->b)->(String->b->b)->(b->b->b)->LineaProd->b
foldProd fMat fAdd fUni t =
        case t of:
           (Materiales xs) -> fMat xs
           (Agregar s p)   -> fAdd s (rec p)
           (Unir p1 p2 )   -> fUni (rec p1) (rec p2)
           where rec =
                 foldProd fMat fAdd fUni


recrProp :: ([String]->b)->(String->LineaProd->b->b)->(LineaProd->LineaProd->b->b->b)-> LineaProd->b
recrProp fMat fAdd fUni t = 
    case t of:
    (Materiales xs) -> fMat xs
    (Agregar s p)   -> fAdd s  p  (rec p)
    (Unir p1 p2 )   -> fUni p1 p2 (rec p1)(rec p2)
    where rec =
          recrProp fMat fAdd fUni


tieneElementosComunes :: [String] -> [String] -> Bool 
tieneElementosComunes m1 m2 = any (\m -> elem m m2) m1



sublineasDisjuntas :: LineaProd -> Bool
sublineasDisjuntas  = recrProd (\_ -> true)
                               (\s _ rec -> rec)
                               (\p1 p2 rec1 rec2 ->
                               let 
                                   m1 = materialesUsados p1
                                   m2 = materialesUsados p2  
                                   rec = not(tieneElementosComunes m1 m2)
                               in 
                                  rec1 && rec2 && rec
                                )


mismaEstructura :: LineaProd -> LineaProd -> Bool
mismaEstructura p1 p2 = 
   case (p1, p2) of
     (Materiales _, Materiales _) -> true
     (Agregar s1 p1, Agregar s2 p2) -> mismaEstructura p1 p2
     (Unir p1 p2, Unir p3 p4) -> mismaEstructura p1 p3 && mismaEstructura p2 p4
      _ -> false
```



