# `debts`

This program allows to split monetry expenses between multiple parties and then calculate the balance - who should pay
how how much. The main use case is travels in a group where there's multiple expenses which should be divided between
people in the group, but which is not convenient to divide at the time of payment - e.g. costs for transportation,
accomodation, food etc.

The whole input data is represneted as one text file in a special format. Recommended extension is `.dbt` (and it's
assumed in the rest of this readme), but it's optional.

## Building

The project uses `cabal`.

    $ cabal compile

## Running

Just provide input file:

    $ ./debts travel.dbt

To provide additional calculation parameters (or overridde the ones defined in the `*.dbt` file):

    $ ./debts travel.dbt -D target.currency=EUR -D round.to=10


## File format

In all following examples there's assumption that four person travel - Kyle, Stan, Eric and Kenny.

### Simple transaction

Erik pays for taxi for the whole group - this is split equally:

    Eric > 100 > Kyle, Stan, Eric, Kenny

Or let's assume it should be split with coefficients:

    Eric > 100 > Kyle*2, Stan

Or there's fixed amounts for someone:

    Eric > 100 > Kyle 60, Stan 40

Or we want to split equally, but add some addition:

    Eric > 100 > Kyle, Eric, Stan +40

We can do the same for left side as well - when multiple people payed for something:

    Eric 60, Kyle 40 > 100 > Kyle*2, Eric 40, Stan +40

### Overall transaction format

    <payees> [ > <amount> ] [ > <beneficators> ] [ @<contragent> ] [ category ] [ tags ] [ : comment ]

where:

    - `payees` - who pays for something.
    - `amount` - how much has been payed. Can be omitted if it can be inferred from `payees` or `beneficators`
    - `beneficators` - how benefits from the payment. Can be omitted if it's the same as `payees` (in this case everyone
      just payed for themselves, so such transaction doesn't affect final balance).
    - `contragent` - entity which actually receives the payment - train company, hotel, store etc. Used just as metadata.
        - internal payments (when one person just gave money to another, e.g. as a part of levelling out the balance)
          are marked with `internal` contragent - such payments don't participate in calculating
          overall expenses.
    - `category` - metadata property for transaction category, can me composite: `(food, restaurant)`.
    - `tags` - metadata property for transaction tags, can me multiple: `[tag1, tag2]`.
    - `comment` - just some comment.


### Groups

Rather sooner than later you'd want to define groups:

    group all = Kyle, Stan, Eric, Kenny
    Eric > 100 > all

Groups can use multipliers too:

    group all_food = Kyle, Stan, Eric*2, Kenny

Groups can be defined using other groups - e.g. to add someone:

    group all_wo_kenny = all, Foo

Or remove someone:

    group all_wo_kenny = all, -Kenny

Or redefine multiplier for someone:

    group all_food = all, =Eric*2


### Currencies

All sums can use currencies:

    A > 100 USD > all

Multiple currencies are allowed everywhere:

    A > 100 USD, 50 EUR > all

You can mix currencies and raw numbers:

    A > 100, 50 EUR > all

You can defined currency for raw numbers:

    param default.currency = USD

    A > 100, 50 EUR > all

Calculations will be done in each currency separately, but you can force conversion to one of them using
`target.currency`:

    param target.currency = USD

or during calculation:

    $ ./debts travel.dbt -D target.currency=USD

For that you need to define FX rates:

    
