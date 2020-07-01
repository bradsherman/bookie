<template>
  <div class="hello">
    <div v-if="!register">
      <h1>Register</h1>
      <div class="md-layout-item md-small-size-100">
        <label>Email</label>
        <input v-model="email" />
      </div>
      <div class="md-layout-item md-small-size-100">
        <label>Name</label>
        <input v-model="name" />
      </div>
      <div class="md-layout-item md-small-size-100">
        <label>Unit Size</label>
        <input v-model.number="unitSize" type="number" />
      </div>
      <div class="md-layout-item md-small-size-100">
        <label>Password</label>
        <input type="password" v-model="password" />
      </div>
      <button v-on:click="tryRegister" class="md-primary">Register</button>
    </div>
    <div v-else>{{ register }}</div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import gql from "graphql-tag";

@Component
export default class Register extends Vue {
  // TODO define type for this (eventually generate from graphql schema)
  register = null;
  email = "";
  name = "";
  unitSize = 0;
  password = "";

  async tryRegister() {
    console.log("Registering!");
    console.log(this.email);
    console.log(this.name);
    console.log(this.unitSize);
    console.log(this.password);
    // TODO better validation and error
    if (this.email === "" && this.password === "") {
      return;
    }
    try {
      const result = await this.$apollo.mutate({
        mutation: gql`
          mutation(
            $email: String!
            $password: String!
            $name: String!
            $unitSize: Float!
          ) {
            register(
              email: $email
              password: $password
              name: $name
              unitSize: $unitSize
            ) {
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
          name: this.name,
          unitSize: this.unitSize,
          password: this.password
        },
        errorPolicy: "all"
      });
      console.log(result);
    } catch (e) {
      console.log("ERROR", e);
    }
  }
}
</script>
