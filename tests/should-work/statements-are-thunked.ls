f x y = "OK";

main = f (case error "case fails" of 0 -> "wat";) (if error "if fails" then "wat" else "wat");
