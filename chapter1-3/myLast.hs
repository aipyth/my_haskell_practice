myLast a = if (length a) > 1 
           then myLast (tail a)
           else a
