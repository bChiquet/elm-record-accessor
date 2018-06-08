Using records in Elm
====================

## What the language offers

The Elm language lets us structure data using extensible records.
A record is a structure containing the fields enumerated in its type :
```elm
type alias Contact = { name : String, city : String }

contact : Contact
contact = { name = "Mark", city = "York" }
```

Elm also defines accessors for the fields:
```elm
contactName : Contact -> String
contactName c = c.name
```

There is also a syntax to change fields of a record, but this syntax doesn't let us change nested records.
Therefore, when you want to modify a record, you have to unbox and rebox the records yourself:
```elm
type alias Contacts = { mark : Contact, paul : Contact }

updateMark : Contacts -> Contacts
updateMark contacts = 
  let 
    mark = contacts.mark
    newMark = { mark | city = "newYork" }
  in
    { contacts | mark =newMark }
```

When working with nested records, this gets annoying quickly. 

## Another method inspired from lenses

Taking inspiration from Haskell's lenses, we can create composable functions that let us update records much more smoothly: 
```elm
type alias Social = {bookings : List Booking, people : Contacts}
type alias Booking = {place : Place, confirmed : Bool, bookName : Name, when : Date}
type alias Contacts = { friends : List Contact, business : List Contact}
type alias Contact = {name : Name, nextMeeting : Date}
type alias Name, Place = String
type alias Data = {planning : Planning}
type alias Planning = {Grid : List String}
```

Each accessor lets us access a field in any record carrying that field:
```elm
lensBusiness : ( a -> a ) -> ({s | business : a} -> {s | business : a})  
lensBusiness change s = { s | business = change s.business }
```

### Modify a nested value

This is still useful, as it lets us modify deeply nested records easily:
```elm
lensPeople : (a -> a) -> ({s | people : a} -> {s | people : a})
lensPeople change s = { s | people = change s.people }

changeJob : Social -> Social
changeJob s = (lensPeople << lensBusiness) (\_ -> []) s
```

We can define a setter and store our functions in a record to make it prettier:
```elm
set : a -> (a -> a)
set a = \_ -> a

l = { people   = lensPeople
    , business = lensBusiness
    -- define all the accessors we need
    }

changeJob : Social -> Social
changeJob s = (l.people << l.business) (set []) s
```

### Chaining, mapping

We can define more operations by adding more `((a -> a) -> (b -> b))` functions that compose with the accessors we already have.
```elm
onEach : (a -> a ) -> (List a -> List a)
onEach f = \a -> List.map f a
```

We can also chain operations simply, with the `|>` operator:
```elm
meetFriendsAt : Booking -> Social -> Social 
meetFriendsAt ({when} as book) soc : 
    soc
    |> (l.bookings) (\books -> book :: books)
    |> (l.people << l.friends << oneEach << l.nextMeeting) (set when)
```

## Drawbacks of this approach

The most annoying thing with this method is that if you only have vanilla Elm you have to write all the accessors yourself.
A solution to bypass this is to generate the accessors for every field name in your codebase and export them in a single record (`l`, in our example).

Another drawback of this method compared to lenses in Haskell, is that you can not apply arbitrary operations on the accessor (get *and* set for instance).
This is possible in Haskell but not in Elm, because the functor abstraction is not available in Elm.