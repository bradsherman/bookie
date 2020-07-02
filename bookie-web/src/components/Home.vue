<template>
  <div class="hello">
    <div v-if="myUserInfo">
      <p>hey {{ myUserInfo.name }}!</p>
      <p>Your unitSize is {{ myUserInfo.unitSize }}</p>
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
            name
            email
            unitSize
          }
        }
      `
    }
  }
})
export default class Home extends Vue {}
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
