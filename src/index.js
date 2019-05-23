import "bulma/css/bulma.min.css";
import "@fortawesome/fontawesome-free/css/all.min.css";

import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

var app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    email: process.env.ELM_APP_EMAIL,
    name: process.env.ELM_APP_NAME
  }
});

registerServiceWorker();
