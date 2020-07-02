<template>
  <div class="page-container">
    <md-app>
      <md-app-toolbar class="md-primary md-layout md-alignment-center-space-between">
        <h3 class="md-title md-layout-item">bookie</h3>
        <div class="md-layout-item">
          <div v-if="isAuthenticated">
            <md-button to="/">Home</md-button>
            <md-button v-on:click="logout" style="color: #fff;">Logout</md-button>
          </div>
          <div v-else>
            <md-button to="/login">Login</md-button>
            <md-button to="/register">Register</md-button>
          </div>
        </div>

        <!--
couldn't get this to work without the links being blue/purple
        <md-tabs md-sync-route>
          <md-tab id="tab-home" class="tab-route" md-label="Home" to="/" exact></md-tab>
          <md-tab id="tab-login" class="tab-route" md-label="Login" to="/login"></md-tab>
          <md-tab id="tab-register" class="tab-route" md-label="Register" to="/register"></md-tab>
          <md-tab id="tab-test" class="tab-route" md-label="Test" to="/test"></md-tab>
        </md-tabs>
        -->
      </md-app-toolbar>
    </md-app>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import IsAuthenticated from "@/IsAuthenticated";
@Component
export default class Navbar extends Vue {
  get isAuthenticated() {
    return IsAuthenticated();
  }
  logout() {
    // TODO bring all token logic into one place
    localStorage.removeItem("token");
    this.$router.push("/login");
  }
}
</script>

<style scoped>
.md-app {
  max-height: 50vh;
  border: 1px solid rgba(rgb(0, 0, 0), 0.12);
  background: linear-gradient(-90deg, #6a79cf, #1627c0);
  color: #ffffff;
}
a {
  color: inherit;
  text-decoration: none;
}
</style>
