const evtSource = new EventSource("events");

evtSource.addEventListener("element", (event) => {
  const {name, state} = JSON.parse(event.data);
  const element = document.getElementById(name);
  element.src = `/element-pics/${name}/${state}`;
});

evtSource.addEventListener('player', (event) => {
  const {id, data, summons, xexpr} = JSON.parse(event.data);
  if (document.querySelector(`#${id}`) !== null) {
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
  } else {
    const players = document.querySelector('ul.creatures');
    players.insertAdjacentHTML('afterbegin', xexpr);
  }
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
    const monsters = document.querySelector('ul.creatures');
    monsters.insertAdjacentHTML('beforeend', xexpr);
  }
});

evtSource.addEventListener('reorder-ids', (event) => {
  const ids = JSON.parse(event.data);
  const creatures = document.querySelector('ul.creatures');
  let newCreatures = [];
  for (const id of ids) {
    newCreatures.push(document.querySelector(`#${id}`));
  }
  creatures.innerHTML = '';
  for (const c of newCreatures) {
    creatures.appendChild(c);
  }
});

evtSource.addEventListener('number', (event) => {
  const {id, n} = JSON.parse(event.data);
  document.querySelector(`#${id}`).innerHTML = n;
});

evtSource.addEventListener('text', (event) => {
  const {id, text} = JSON.parse(event.data);
  document.querySelector(`#${id}`).innerHTML = text;
});

evtSource.addEventListener('alert', (event) => {
  const text = JSON.parse(event.data);
  showDialog(text);
});

let dialog;
function showDialog(text) {
  if (!dialog) {
    dialog = document.getElementById('dialog');
    if (!dialog) return;
  }
  dialog.innerHTML = text;
  dialog.showModal();
  setTimeout(function () {
    dialog.close();
  }, 5000);
}
