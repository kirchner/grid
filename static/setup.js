window.addEventListener("load", function() {
  var app = Elm.Main.init({
    flags: {
      width: window.innerWidth,
      height: window.innerHeight
    }
  });
});
