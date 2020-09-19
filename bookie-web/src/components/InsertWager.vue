<template>
  <div>
    <div>The wager type ids are 1 (Moneyline), 2 (O/U), and 3 (Spread).</div>
    <form novalidate class="md-layout md-alignment-top-center" @submit.prevent="validateWager">
      <md-card class="md-layout-item md-size-50 md-small-size-100">
        <md-card-header>
          <div class="md-title">Insert Wager</div>
        </md-card-header>

        <md-card-content>
          <div class="md-layout md-gutter">
            <div class="md-layout-item md-small-size-100">
              <md-field>
                <label for="wager-type">Wager Type Id</label>
                <md-input
                  name="wager-type"
                  type="number"
                  id="wager-type"
                  v-model="form.wagerType"
                  :disabled="sending"
                />
                <span class="md-error" v-if="!form.wagerType.required">Wager Type Id is required</span>
              </md-field>
            </div>
            <div class="md-layout-item md-small-size-100">
              <md-field>
                <label for="line">Line</label>
                <md-input name="line" id="line" v-model="form.line" :disabled="sending" />
                <span class="md-error" v-if="!form.line.required">Line is required</span>
              </md-field>
            </div>
            <div class="md-layout-item md-small-size-100">
              <md-field>
                <label for="odds">Odds</label>
                <md-input name="odds" id="odds" v-model="form.odds" :disabled="sending" />
                <span class="md-error" v-if="!form.odds.required">Odds are required</span>
              </md-field>
            </div>
          </div>
          <div class="md-layout md-gutter">
            <div class="md-layout-item md-small-size-100">
              <md-field>
                <label for="bettor-id">Bettor Id</label>
                <md-input
                  name="bettor-id"
                  type="number"
                  id="bettor-id"
                  v-model="form.bettorId"
                  :disabled="sending"
                />
                <span class="md-error" v-if="!form.bettorId.required">Bettor Id is required</span>
              </md-field>
            </div>
            <div class="md-layout-item md-small-size-100">
              <md-field>
                <label for="offerer-id">Offerer Id</label>
                <md-input
                  name="offerer-id"
                  type="number"
                  id="offerer-id"
                  v-model="form.offererId"
                  :disabled="sending"
                />
                <span class="md-error" v-if="!form.offererId.required">Offerer Id is required</span>
              </md-field>
            </div>
            <div class="md-layout-item md-small-size-100">
              <md-field>
                <label for="amount">Amount</label>
                <md-input
                  name="amount"
                  type="number"
                  id="amount"
                  v-model="form.amount"
                  :disabled="sending"
                />
                <span class="md-error" v-if="!form.amount.required">Odds are required</span>
              </md-field>
            </div>
          </div>
          <div class="md-layout md-gutter">
            <div class="md-layout-item md-small-size-100">
              <md-field>
                <label for="description">Description</label>
                <md-textarea
                  name="description"
                  id="description"
                  v-model="form.description"
                  :disabled="sending"
                />
                <span class="md-error" v-if="!form.description.required">Description is required</span>
              </md-field>
            </div>
          </div>
        </md-card-content>
        <md-progress-bar md-mode="indeterminate" v-if="sending" />
        <md-card-actions>
          <md-button type="submit" class="md-raised md-primary" :disabled="sending">Insert Wager</md-button>
        </md-card-actions>
      </md-card>
      <md-snackbar :md-active.sync="wagerSaved">The wager was saved with success!</md-snackbar>
    </form>
    <div v-if="errorMessage != ''">{{ errorMessage }}</div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import gql from "graphql-tag";
export default class InsertWager extends Vue {
  sending = false;
  wagerSaved = false;
  errorMessage = "";
  form = {
    wagerType: -1,
    odds: 0,
    line: 0,
    bettorId: this.$store.state.user.id,
    offererId: -1,
    amount: 0.0,
    description: ""
  };
  async validateWager() {
    console.log(this.form);
    console.log(this.errorMessage);
    if (this.form.wagerType == -1) {
      console.log("hello");
      this.errorMessage = "Please fill out wagerType";
      return;
    }
    // TODO more validation with vuelidate or some other lib

    this.sending = true;
    const details = {
      odds: Number(this.form.odds),
      line: Number(this.form.line)
    };
    const detailsJSON = JSON.stringify(details);
    const result = await this.$apollo.mutate({
      mutation: gql`
        mutation(
          $wagerType: Int!
          $wagerDetails: String!
          $bettorId: Int!
          $offererId: Int!
          $amount: Float!
          $description: String!
        ) {
          addWager(
            wagerType: $wagerType
            wagerDetails: $wagerDetails
            bettorId: $bettorId
            offererId: $offererId
            amount: $amount
            description: $description
          )
        }
      `,
      variables: {
        wagerType: Number(this.form.wagerType),
        wagerDetails: detailsJSON,
        bettorId: Number(this.form.bettorId),
        offererId: Number(this.form.offererId),
        amount: Number(this.form.amount),
        description: this.form.description
      },
      errorPolicy: "all"
    });
    const { data, errors } = result;
    console.log(result);
    this.sending = false;
    if (data) {
      this.wagerSaved = true;
      /* } else { */
      /*   this.errorMessage = err */
    }
  }
}
</script>

<style scoped>
</style>
