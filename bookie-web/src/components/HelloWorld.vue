<template>
  <div class="hello">
    <p>hey</p>
    <p>{{ hello }}</p>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import gql from "graphql-tag";

@Component({
  apollo: {
    hello: {
      query: gql`
        query {
          hello
        }
      `
    }
  }
})
export default class HelloWorld extends Vue {}
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
