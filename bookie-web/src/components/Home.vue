<template>
  <div class="hello">
    <div v-if="myUserInfo">
      <p>hey {{ myUserInfo.firstName }}!</p>
      <p>Your id is {{ myUserInfo.id }} and unitSize is {{ myUserInfo.unitSize }}</p>
    </div>
    <InsertWager />
    <MyWagers />
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import gql from "graphql-tag";

import InsertWager from "./InsertWager.vue";
import MyWagers from "./MyWagers.vue";

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
  },
  components: {
    InsertWager,
    MyWagers
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
