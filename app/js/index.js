export function sayHello(name) {
  console.log(`Hello ${name}!`);
}

Shiny.addCustomMessageHandler('sayHello', sayHello);