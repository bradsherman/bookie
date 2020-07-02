export default () => {
  const token = localStorage.getItem("token");
  return !!token;
};
