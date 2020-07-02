<template>
  <div class="md-layout md-alignment-top-center">
    <form class="login md-layout-item md-size-50" @submit.prevent="tryLogin">
      <h1>Login</h1>
      <div class="md-small-size-100">
        <md-field>
          <label for="email">Email</label>
          <md-input name="email" id="email" v-model="email" />
        </md-field>
      </div>
      <div class="md-small-size-100">
        <md-field>
          <label for="password">Password</label>
          <md-input name="password" type="password" id="password" v-model="password" />
        </md-field>
      </div>
      <md-button class="md-raised md-accent" type="submit">Login</md-button>
      <div v-if="errorMessage != ''" class="errorMsg md-elevation-1">{{ errorMessage }}</div>
    </form>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import gql from "graphql-tag";

@Component
export default class Login extends Vue {
  // TODO define type for this (eventually generate from graphql schema)
  login = null;
  email = "";
  password = "";
  errorMessage = "";

  async tryLogin() {
    if (this.email === "" || this.password === "") {
      this.errorMessage = "Please fill out the email and password fields.";
      return;
    }
    try {
      const result = await this.$apollo.query({
        query: gql`
          query($email: String!, $password: String!) {
            login(email: $email, password: $password) {
              token
              user {
                id
                name
                email
                unitSize
              }
            }
          }
        `,
        variables: {
          email: this.email,
          password: this.password
        },
        errorPolicy: "all"
      });
      const { data, errors } = result;
      if (data) {
        const { token, user } = data.login;
        localStorage.setItem("token", token);
        this.$router.push({ name: "Home", params: { token, user } });
      } else {
        if (errors && errors.length > 0) {
          this.errorMessage = errors[0].message.split(":")[1];
        } else {
          this.errorMessage = "Unknown error occurred.";
        }
      }
    } catch (e) {
      this.errorMessage = "Unknown error occurred.";
    }
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
.login {
  padding-left: 20px;
  width: "50%";
}
.errorMsg {
  margin-top: 20px;
  background-color: indianred;
  font-weight: bold;
  vertical-align: "center";
  color: #2c3e50;
}
</style>
