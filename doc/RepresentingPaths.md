# Representing Paths

This document is only concerned with a small part of the "filepath/URI" problem, and thats getting a value from the user.

## The Crux

The user is going to be giving us Text, and we will ultimately be sending Text to the server

So however we represent it in the interim (after we get it from the user and before we send it off to whoever) should be compared to
the minimal solution.

## The Minimal Solution

We're getting it as Text from the user and sending it off as Text,

So the minimal solution is to simply represent it internally with `T.Text`. Get it from the user and pass it onto the server.

It has the following pros:

-   Some error checking. There is server side error checking that goes on with the input that we can relay back to the user in case of a failure (I only say "Some" because i'm not sure how extensive the error checking is likely to get)
-   Simple.  It's easy to say what it is and since we are doing nothing to the user input its easy to enumerate what it delivers.
-   Represents the users input perfectly.  For better or worse, what gets passed to the server will completely communicate the users intent (at least to the extent that it has been communicated to us).  Mistakes and all.
    -   If we begin to do stuff like remove `/`s (for instance, it might be favourable to remove double slashes, if we deem that unlikely for a user to ever wish to use intentionally) we risk removing stuff that the user added intentionally, like potentially trailing/leading slashes which can drastically alter the intent of path.


Cons:

-   Whatever "mistakes" don't get caught by the server-side error checking will result in unexpected changes to a server side resource that we will either have to live with or correct.  Depending how reliant the rest of the program was on the outcome, it may happen silently too.
-   No Structure. Without additional structure, it might be a bit of a pain to supplement the server side error checking with our own checks.
-   No type safety when *we* create values or combine other user inputs to form a path.
    -   This is probably the biggest point for not using plain old Text.  If we are splicing together bits of user input to generate a path, we are then free (?) to do so according to our own standards or conventions, at which point you might want to make sure you dont introduce double slashes and what not.
    -   While representing this as `Text`, its trickier (but of course doable) to do this without making mistakes (and be resilient to changes that might make these mistakes in the future).

## A possible next step to a better solution

Some code for this can be seen on the `topic/prefix-list`.  But an attempt at doing something just a "little bit" more than using raw text can be seen in this change for `Key` to be wrapped around something *like* `[Text]` instead of just `Text`.

(PR [here](https://github.com/ambiata/mismi/pull/6#issuecomment-76651465))

More specfically it introduced this type:

``` Haskell
data Component = Component { _unComponent :: T.Text } deriving (Eq, Typeable, Data)
```

Where semantically `Component` is the part of the S3 URL after the bucket/domain and in between the slashes.  At the time of this writing, the only constraint it placed over `Text` was that it was non-empty and contained no slashes.

```
s3://somebucket/foo/bar/bubble.txt
                ^   ^   ^
                |   |   |
Components --------------
```

And `Key` was changed to look like so:

``` Haskell
data Key =
        EmptyKey
    |   Key :/ Component
```

`Address` didnt change in its relationship to `Key`.

``` Haskell
-- reminder
data Address = Address
    {   bucket  :: Bucket
    ,   key     :: Key
    }   deriving (Eq, Show)
```

Combinators can be provided for these types to allow us to manipulate values and generate valid values from other valid values, being the primary "pro" for the change.

The only positive value that comes from this change as it is, is that we can produce our own paths from smaller components with more confidence that we dont have things that we dont want, like double slashes (putting aside the concern about the trailing characters...).

Its also considered important that there exists a total function `parseKey :: T.Text -> Key`.  That is, every value of `Text` is a valid `Key`. Which is problematic in terms of deciding where the empty `Text` case gets represented. Both `Component` and `Key` as concepts have arguments for why emptiness is not in their jurisdiction.

So since `parseKey` is effectively [`wordsBy (== '/')`](https://hackage.haskell.org/package/split-0.1.2/docs/Data-List-Split.html#v:wordsBy) (i.e split on `/` and discard empty words) we get the following behaviour when comparing the text provided by the user and the final text sent in the S3 operations:

1.  multiple slashes get collapsed into one.
2.  trailing/leading slashes get dropped.

Point 1. can be considered positive, but point 2. is particularly potent as it means the user could provide input with a slash at the end that is intended to point to a directory location but we drop that information and store something that could represent an object address.

So whatever the final solution is, we would want to protect against this possibility (See [here](https://github.com/ambiata/mismi/issues/7) for the `mismi` issue).

This would be fixed with the addition of more types, however with Point 2, this is a bad intermediate state to leave things at.

Again with the addition of more types to replace `Address` we could have the benefit of more error checking, without having to resort to IO to get the server to check it.  But this benefit isnt realised yet with this change.

## Take home points

-   The pros for the super simple solution (just plain text) should be the bare minimum as far as evaluating other solutions and intermediate states when working towards building up an implementation of a good solution.
-   Perhaps we could consider as a property for `parseKey :: T.Text -> Key` and `showKey :: Key -> T.Text` that `showKey . parseKey == id`, so we may have a different internal representation for these things so we can manipulate and create values of them, but in cases where we just get the value from the user and pass it on, there is nothing added/dropped to interfere with any intent that wanted to convey with the input as they gave it.
    -   Its this property that seems to be where most of the benefits for the raw text solution come from.
    -   Following on from this point, rather than filter out things like double slashes, we could just ensure that they never behave the same way (See `mismi` issue [here](https://github.com/ambiata/mismi/issues/8)).
