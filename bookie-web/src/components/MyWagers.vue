<template>
  <div>
    <div v-for="wager in myWagers" v-bind:key="wager.id">
      {{ wager.bettor.firstName }} bet {{ wager.offerer.firstName }} a {{ wager.wagerType.name }} for {{ wager.amount }}
      with odds {{ wager.details.odds }}
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import gql from "graphql-tag";

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
export default class MyWagers extends Vue {}
</script>

<style scoped>
</style>
