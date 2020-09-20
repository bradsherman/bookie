<template>
  <div class="md-layout md-alignment-center-center">
    <div class="md-layout-item">
      <div class="md-layout-item md-layout md-alignment-center-center md-gutter">
        <h3 class="md-alignment-center-center">My Wagers</h3>
        <md-button v-on:click="toggleInsertWager" class="md-icon-button md-raised">
          <md-icon v-if="!this.$props.showInsertWager">add</md-icon>
          <md-icon v-else>minus</md-icon>
        </md-button>
      </div>
      <div v-for="wager in myWagers" v-bind:key="wager.id">
        {{ wager.bettor.firstName }} bet {{ wager.offerer.firstName }} a {{ wager.wagerType.name }} for {{ wager.amount }}
        with odds {{ wager.details.odds }}
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import gql from "graphql-tag";

const MyWagersProps = Vue.extend({
  props: {
    showInsertWager: Boolean,
    toggleInsertWager: Function
  }
});

@Component({
  apollo: {
    myWagers: {
      query: gql`
        query {
          myWagers {
            id
            wagerType {
              id
              name
            }
            details {
              odds
              line
            }
            bettor {
              firstName
              lastName
            }
            offerer {
              firstName
              lastName
            }
            amount
          }
        }
      `
    }
  }
})
export default class MyWagers extends MyWagersProps {}
</script>

<style scoped>
</style>
