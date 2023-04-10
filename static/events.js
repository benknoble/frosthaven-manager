const evtSource = new EventSource("events");

evtSource.addEventListener("element", (event) => {
  const data = JSON.parse(event.data);
  const element = document.getElementById(data.name);
  element.src = `/element-pics/${data.name}/${data.state}`;
});
