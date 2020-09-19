<template>
  <div>
    <div v-if="myUserInfo" class="md-primary md-layout md-alignment-center-center">
      <div class="md-layout-item md-alignment-top-center">
        <h2>Profile Information</h2>
        <md-list>
          <md-list-item>
            <span class="md-list-item-text">First Name:</span>
            {{ myUserInfo.firstName }}
          </md-list-item>

          <md-list-item>
            <span class="md-list-item-text">Last Name:</span>
            {{ myUserInfo.lastName }}
          </md-list-item>

          <md-list-item>
            <span class="md-list-item-text">ID:</span>
            {{ myUserInfo.id }}
          </md-list-item>

          <md-list-item>
            <span class="md-list-item-text">Email:</span>
            {{ myUserInfo.email }}
          </md-list-item>

          <md-list-item>
            <span class="md-list-item-text">Unit size:</span>
            {{ myUserInfo.unitSize }}
          </md-list-item>
        </md-list>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import gql from "graphql-tag";

@Component({
  apollo: {
    myUserInfo: {
      query: gql`
        query {
          myUserInfo {
            id
            firstName
            lastName
            email
            unitSize
          }
        }
      `
    }
  }
})
export default class Profile extends Vue {}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
h3 {
  margin: 40px 0 0;
}
ul {
  list-style-type: none;
  padding: 0;
}
li {
  display: inline-block;
  margin: 0 10px;
}
a {
  color: #42b983;
}
</style>
