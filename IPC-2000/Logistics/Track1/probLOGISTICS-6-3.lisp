(defproblem LOGISTICS-6-3 LOGISTICS
  (
    ;;;
    ;;;  Facts
    ;;;
    (AIRPLANE APN1)
    (AIRPORT APT1)
    (AIRPORT APT2)
    (LOCATION POS1)
    (LOCATION POS2)
    (CITY CIT1)
    (CITY CIT2)
    (TRUCK TRU1)
    (TRUCK TRU2)
    (PACKAGE OBJ11)
    (PACKAGE OBJ12)
    (PACKAGE OBJ13)
    (PACKAGE OBJ21)
    (PACKAGE OBJ22)
    (PACKAGE OBJ23)
    ;;;
    ;;;  Initial states
    ;;;
    (AT APN1 APT2)
    (AT TRU1 POS1)
    (AT OBJ11 POS1)
    (AT OBJ12 POS1)
    (AT OBJ13 POS1)
    (AT TRU2 POS2)
    (AT OBJ21 POS2)
    (AT OBJ22 POS2)
    (AT OBJ23 POS2)
    (IN-CITY POS1 CIT1)
    (IN-CITY APT1 CIT1)
    (IN-CITY POS2 CIT2)
    (IN-CITY APT2 CIT2)
  )
  ;;;
  ;;;  Goals (task list)
  ;;;
  (:unordered 
    (:task AT OBJ12 POS1)
    (:task AT OBJ23 POS1)
    (:task AT OBJ11 APT1)
    (:task AT OBJ22 APT1)
    (:task AT OBJ13 POS2)
    (:task AT OBJ21 POS2)
  )
)
