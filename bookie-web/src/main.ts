import Vue from "vue";
import VueApollo from "vue-apollo";
import VueMaterial from "vue-material";
import "vue-material/dist/vue-material.min.css";

import App from "./App.vue";
import apolloClient from "./apolloClient";

Vue.use(VueApollo);
Vue.use(VueMaterial);
Vue.config.productionTip = false;

const apolloProvider = new VueApollo({
  defaultClient: apolloClient
});

new Vue({
  el: "#app",
  apolloProvider,
  render: h => h(App)
});
