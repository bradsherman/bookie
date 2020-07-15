<template>
  <div class="md-layout md-alignment-top-center">
    <form class="md-layout-item md-size-50" @submit.prevent="tryRegister">
      <h1>Register</h1>
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
      <div class="md-small-size-100">
        <md-field>
          <label for="firstName">First Name</label>
          <md-input name="firstName" id="firstName" v-model="firstName" />
        </md-field>
        <md-field>
          <label for="lastName">Last Name</label>
          <md-input name="lastName" id="lastName" v-model="lastName" />
        </md-field>
      </div>
      <div class="md-small-size-100">
        <md-field>
          <label for="unitSize">Unit Size</label>
          <md-input name="unitSize" id="unitSize" v-model.number="unitSize" />
        </md-field>
      </div>
      <md-button class="md-raised md-primary" type="submit">Register</md-button>
      <div v-if="errorMessage != ''" class="errorMsg md-elevation-1">{{ errorMessage }}</div>
    </form>
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
  firstName = "";
  lastName = "";
  unitSize = 0.0;
  password = "";
  errorMessage = "";

  async tryRegister() {
    // TODO better validation and error
    if (this.email === "" || this.password === "") {
      this.errorMessage = "Please fill out all fields.";
      return;
    }
    try {
      const result = await this.$apollo.mutate({
        mutation: gql`
          mutation(
            $email: String!
            $password: String!
            $firstName: String!
            $lastName: String!
            $unitSize: Float!
          ) {
            register(
              email: $email
              password: $password
              firstName: $firstName
              lastName: $lastName
              unitSize: $unitSize
            ) {
              token
              user {
                id
                firstName
                lastName
                email
                unitSize
              }
            }
          }
        `,
        variables: {
          email: this.email,
          firstName: this.firstName,
          lastName: this.lastName,
          unitSize: this.unitSize,
          password: this.password
        },
        errorPolicy: "all"
      });
      const { data, errors } = result;
      console.log(result);
      if (data) {
        const { token, user } = data.register;
        this.$store.commit("login", {
          token,
          user
        });
        this.$router.push({ name: "Home", params: { token, user } });
      } else {
        if (errors && errors.length > 0) {
          this.errorMessage = errors[0].message.split(":")[1];
        } else {
          this.errorMessage = "Unknown error occurred.";
        }
      }
    } catch (e) {
      console.log(e);
      this.errorMessage = "Unknown error occurred.";
    }
  }
}
</script>

<style scoped>
.errorMsg {
  margin-top: 20px;
  background-color: indianred;
  font-weight: bold;
  vertical-align: "center";
  color: #2c3e50;
}
</style>
