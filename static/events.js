const evtSource = new EventSource("events");

evtSource.addEventListener("element", (event) => {
  const {name, state} = JSON.parse(event.data);
  const element = document.getElementById(name);
  element.src = `/element-pics/${name}/${state}`;
});

evtSource.addEventListener('player', (event) => {
  const {id, data, summons} = JSON.parse(event.data);
  for (const css_class in data) {
    const element = document.querySelector(`#${id} .${css_class}`);
    const d = data[css_class];
    const typ = typeof d;
    if (typ === 'string') {
      element.innerHTML = d;
    } else if (typ === 'object') {
      for (const attr in d) {
        element[attr] = d[attr];
      }
    }
  }
  summons_element = document.querySelector(`#${id} ol.summons`);
  summons_element.innerHTML = summons.join('');
});

evtSource.addEventListener('monster-group', (event) => {
  const {id, data, xexpr} = JSON.parse(event.data);
  if (document.querySelector(`#${id}`) !== null) {
    for (const css_class in data) {
      const element = document.querySelector(`#${id} .${css_class}`);
      const d = data[css_class];
      const typ = typeof d;
      if (typ === 'string') {
        element.innerHTML = d;
      } else if (typ === 'object' && Array.isArray(d)) {
        element.innerHTML = d.join('');
      }
    }
  } else {
    const monsters = document.querySelector('ul.monsters');
    monsters.insertAdjacentHTML('beforeend', xexpr);
  }
});

evtSource.addEventListener('number', (event) => {
  const {id, n} = JSON.parse(event.data);
  document.querySelector(`#${id}`).innerHTML = n;
});
