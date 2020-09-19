<template>
  <div class="md-layout">
    <div class="md-layout-item">
      <div class="md-layout-item" v-if="showInsertWager">
        <InsertWager v-bind:toggleInsertWager="toggleInsertWager" />
      </div>
      <MyWagers
        v-bind:showInsertWager="showInsertWager"
        v-bind:toggleInsertWager="toggleInsertWager"
      />
    </div>
  </div>
</template>

<script lang="ts">
// TODO: re-organize my wagers and insert, it's clunky
// maybe use FAB with insert wager modal
import { Component, Vue } from "vue-property-decorator";
import gql from "graphql-tag";

import InsertWager from "./InsertWager.vue";
import MyWagers from "./MyWagers.vue";

@Component({
  components: {
    InsertWager,
    MyWagers
  }
})
export default class Home extends Vue {
  showInsertWager = false;
  toggleInsertWager() {
    this.showInsertWager = !this.showInsertWager;
  }
}
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
