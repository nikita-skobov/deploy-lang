function USER_FUNCTION_NAME(input, output, name) {

}
// user function gets inlined above ^


const out = USER_FUNCTION_NAME(
    JSON.parse(process.env.RESOURCE_INPUT),
    JSON.parse(process.env.RESOURCE_OUTPUT),
    process.env.RESOURCE_NAME,
);
console.log(JSON.stringify(out))
