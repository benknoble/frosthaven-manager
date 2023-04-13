const evtSource = new EventSource("events");

evtSource.addEventListener("element", (event) => {
  const data = JSON.parse(event.data);
  const element = document.getElementById(data.name);
  element.src = `/element-pics/${data.name}/${data.state}`;
});

evtSource.addEventListener('player', (event) => {
  const data = JSON.parse(event.data);
  const id = data.id;
  for (const css_class in data.data) {
    document.querySelector(`#${id} > .${css_class}`).innerHTML = data.data[css_class];
  }
});
