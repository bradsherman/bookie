import Vue from "vue";
import Vuex from "vuex";

Vue.use(Vuex);

interface BookieUser {
  email: string;
  id: number;
  name: string;
  unitSize: number;
}

interface BookieState {
  authToken: string | null;
  user: BookieUser | null;
}

const user: string | null = localStorage.getItem("user");
const bUser: BookieUser | null = user && JSON.parse(user);
const state: BookieState = {
  authToken: localStorage.getItem("token"),
  user: bUser
};

interface LoginPayload {
  token: string;
  user: BookieUser;
}

export default new Vuex.Store({
  state,
  getters: {
    token: (state: BookieState): string | null => {
      console.log(state);
      if (state.authToken) {
        return state.authToken;
      } else {
        return null;
      }
    },
    isAuthenticated: (state: BookieState): boolean => {
      return state.authToken != null;
    }
  },
  mutations: {
    login: (state: BookieState, payload: LoginPayload) => {
      localStorage.setItem("token", payload.token);
      localStorage.setItem("user", JSON.stringify(payload.user));
      state.authToken = payload.token;
      state.user = payload.user;
    },
    logout: (state: BookieState) => {
      localStorage.removeItem("token");
      localStorage.removeItem("user");
      state.authToken = null;
    }
  }
});
