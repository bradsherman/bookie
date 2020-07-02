import Vue from "vue";
import VueRouter, { RouteConfig } from "vue-router";
import Home from "../components/Home.vue";
import Login from "../components/Login.vue";
import Register from "../components/Register.vue";

import IsAuthenticated from "../IsAuthenticated";

Vue.use(VueRouter);

const checkNoAuth = (to: any, from: any, next: any) => {
  if (!IsAuthenticated()) {
    next();
    return;
  }
  next("/");
};

const checkAuth = (to: any, from: any, next: any) => {
  if (IsAuthenticated()) {
    next();
    return;
  }
  next("/login");
};

const routes: Array<RouteConfig> = [
  {
    path: "/",
    name: "Home",
    component: Home,
    beforeEnter: checkAuth
  },
  {
    path: "/login",
    name: "Login",
    component: Login,
    beforeEnter: checkNoAuth
  },
  {
    path: "/register",
    name: "Register",
    component: Register,
    beforeEnter: checkNoAuth
  }
];

const router = new VueRouter({
  mode: "history",
  base: process.env.BASE_URL,
  routes
});

export default router;
