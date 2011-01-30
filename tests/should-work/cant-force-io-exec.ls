import io;

print x = case _jsfun "(function(x) {print(x); return 0;})" 1 x of 0 -> return ();;

printHej = print "hej";

main = do {
    case printHej of _ -> return ();;
    case printHej of _ -> return ();;
    case print "hej" of _ -> return ();;
    return "OK";
  };
