import { ApolloClient } from "apollo-client";
import { ApolloLink } from "apollo-link";
import { createHttpLink } from "apollo-link-http";
import { InMemoryCache } from "apollo-cache-inmemory";

/* eslint-disable */
// HTTP connection to the API
const httpLink = createHttpLink({
  // You should use an absolute URL here
  uri: "http://localhost:8000/api",
});

const authMiddleware = new ApolloLink((operation, forward) => {
  const token = localStorage.getItem("token");
  if (token) {
    operation.setContext({
      headers: {
        authorization: token ? `Bearer ${token}` : "",
      },
    });
  }
  return forward(operation);
});

// Cache implementation
const cache = new InMemoryCache();

// Create the apollo client
const apolloClient: any = new ApolloClient({
  link: authMiddleware.concat(httpLink),
  cache,
  connectToDevTools: true,
});

export default apolloClient;
