import io;
import canvas;

paint 255 _ = return ();
paint n can = do {
    fillColor can (RGB n 0 n);
    fillRect can (Pt n n) (Pt (512-2*n) (512-2*n));
    paint (n+1) can;
  };

paintGradient = withCanvasDo "canvas" (paint 0);
