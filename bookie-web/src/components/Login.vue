<template>
  <div class="hello">
    <div v-if="!login">
      <h1>Login</h1>
      <div class="md-layout-item md-small-size-100">
        <label>Email</label>
        <input v-model="email" />
      </div>
      <div class="md-layout-item md-small-size-100">
        <label>Password</label>
        <input type="password" v-model="password" />
      </div>
      <button v-on:click="tryLogin" class="md-primary">Login</button>
    </div>
    <div v-else>{{ login }}</div>
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

  async tryLogin() {
    console.log("Logging in!");
    console.log(this.email);
    console.log(this.password);
    if (this.email !== "" && this.password !== "") {
      console.log(this.login);
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
      console.log(result);
      /* const { data, errors, loading } = result; */
      /* console.log(result.errors); */
      /* console.log(result); */
      /* this.login = result.data.login; */
      /*   const result = await this.$apollo.mutate({ */
      /*     mutation: gql` */
      /*       mutation($email: String!, $password: String!) { */
      /*         login(email: $email, password: $password) { */
      /*           token */
      /*           user { */
      /*             id */
      /*             name */
      /*             email */
      /*             unitSize */
      /*           } */
      /*         } */
      /*       } */
      /*     `, */
      /*     variables: { */
      /*       email: this.email, */
      /*       password: this.password */
      /*     } */
      /*   }); */
      /*   console.log(result); */
    } catch (e) {
      console.log("ERROR", e);
    }
  }
}
</script>
