const evtSource = new EventSource("events");

evtSource.addEventListener("element", (event) => {
  const {name, state} = JSON.parse(event.data);
  const element = document.getElementById(name);
  element.src = `/element-pics/${name}/${state}`;
});

evtSource.addEventListener('player', (event) => {
  const {id, data} = JSON.parse(event.data);
  for (const css_class in data) {
    document.querySelector(`#${id} .${css_class}`).innerHTML = data[css_class];
  }
});

evtSource.addEventListener('number', (event) => {
  const {id, n} = JSON.parse(event.data);
  document.querySelector(`#${id}`).innerHTML = n;
});
