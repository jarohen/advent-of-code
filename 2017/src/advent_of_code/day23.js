// pseudo-code for the assembly in d23

var h = 0;

for (var b = 106500; b < 123500; b += 17) {
  var f = true;

  for (var d = 2; d < b; d++) {
    for (var e = 2; e < b; e++) {
      if (d * e == b) {
        f = false;
      }
    }
  }

  if (!f) {
    h = h + 1;
  }
}
