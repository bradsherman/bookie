import Vue from "vue";
import VueApollo from "vue-apollo";
import VueMaterial from "vue-material";
import "vue-material/dist/vue-material.min.css";

import App from "./App.vue";
import apolloClient from "./apolloClient";
import router from "./router";

Vue.use(VueApollo);
Vue.use(VueMaterial);
Vue.config.productionTip = false;

const apolloProvider = new VueApollo({
  defaultClient: apolloClient
});

new Vue({
  el: "#app",
  apolloProvider,
  router,
  render: h => h(App)
});
