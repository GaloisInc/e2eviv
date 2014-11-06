# Hack-a-PDF Script

## TODOs

- Discuss threat profile of existing elections vs. digital elections.

## Joe's version 5

Narration:

[speaking to camera]

Hi, I’m Joe Kiniry, a security and elections researcher here at
Galois. (5s)

Election Day is today.  If you took advantage of early voting, or you
live overseas, you probably used a paper ballot you received in the
mail a few weeks ago.  A digital alternative, being considered across
the USA, is voting-by-email. (13s)

[Animation/Illustration]

To vote using email, you download and fill out a ballot on your
computer and then email it back to election officials. Sending an
email is meant to be like putting your vote into a ballot box. (9s)

This kind of system would be convenient for voters and officials as it
ensures ballots are filled out properly, permits disabled voters to
vote independently, and ballot counting is quick and accurate. (11s)

Unfortunately, this idea has serious security flaws: It permits a
single hacker to remotely manipulate the outcome of any election. (8s)

When you download a file---like a ballot---or send an email---such as
a vote--your data flows through many untrusted computer systems. For
example, your ballot can be intercepted on the way to you, viruses on
your computer can manipulate your vote without your knowledge, or your
vote can be modified while on its return trip to the government. Any
of these attacks can change the outcome of an entire election, since
the hacker can control ballot distribution, vote choice, and ballot
submission. (25s)

[Illustration/split-screen]

This is not just a theoretical danger. At Galois, we have demonstrated
that a normal wireless router---like the one your ISP installed your
home---can be taken over from anywhere in the world.  By tweaking your
router's software, you and your family's votes are silently changed
after they leave your computer, and before they reach election
officials. What's more, there is no trace of foul play, and the attack
can be automated.  For example, hackers could target a critical number
of voters supporting a particular candidate in a close race, thus
tipping the election whichever way they---or their
customer---wants. (33s)

[speaking to camera]

Despite its presumed benefits, voting by email is deeply flawed. Our
demonstration only took a few days to develop, and is very difficult
to detect, even for security experts. Printing a ballot and mailing it
through postal services, or putting it in a ballot drop, is still the
most secure and reliable solution for early and absentee voting. (18s)

See you next time!

|Galois|


## Shpat's version 4

Narration:

[speaking to camera]

Hi, I’m Joe Kiniry, XXXX here at Galois.

In political elections, remote voting options for citizens are mostly
limited to mail-in ballots. One possible suggested alternative has
been voting by email. To vote remotely, you would fill out a PDF on
your computer and email it to election officials.

This system would be convenient for citizens and would help automate
the counting process for election officials. However, the system also
has serious security flaws. It can be very easily hijacked and we do
not recommend using it for any kind of binding elections.

[Animation/Illustration]

When you download a file or send an email, the data flows through many
untrusted computer systems. The ballot can be intercepted or modified
on the way to the voter, viruses on the voter’s computer can change
the vote, or the vote can be changed on its return trip to the
government. Any of these attacks can change the outcome of an entire
election.

[Illustration/split-screen]

This is not just a theoretical danger. We have demonstrated that a
normal home wireless router can be taken over from anywhere on the
Internet.  A malicious hacker can take over your router in such a way
that it modifies emailed votes after they leave your computer and
before they reach the election official. Your vote for A can arrive as
a vote for B, with no trace of foul play.

[speaking to camera]

Despite its benefits, voting by email is deeply flawed. The
demonstration took only a few days to develop, and could easily be
made very difficult to detect, even for security experts. Printing a
PDF ballot and mailing it through postal services is still a more
secure and reliable solution to absentee voting.  See you next time.

(Galois)

## Daniel's version 3

Many governments are considering how to incorporate computers in their
voting systems, lured in part by the promise of cheap, accurate vote
counting. One way is to have voters fill out a ballot using standard
software and return it to the government by email.

There is a lot to like about this system. Voters can vote from the
comfort of their own home, and email is much faster than regular mail
for returning the ballot. Email software is familiar and inexpensive,
and potentially makes voting accessible to some people who cannot vote
today. Counting votes can be automated, decreasing costs and human
mistakes.

Unfortunately, the system also has serious security flaws. When you
download a file or send an email, the data flows through many
untrusted computer systems. The ballot can be intercepted or modified
on the way to the voter, viruses on the voter’s computer can change
the vote, or the vote can be changed on its return trip to the
government. If any one of these attacks works, it can change the
outcome of an entire election.

To prove that this is not just a theoretical danger, we have
demonstrated that a normal home wireless router can be taken over from
anywhere on the Internet. Afterwards, the router will silently modify
emailed votes after they leave the voter’s computer but before they
arrive at the government. The demonstration took only a few days to
develop, and could easily be made very difficult to detect, even for
security experts.

Despite its benefits, voting by email is deeply flawed. We must demand
a system where we can be confident that our votes are counted. Stay
tuned for a demonstration.

## David's version 2

Postal voting today is difficult and slow. Voting by email is one way
to improve this process, and it could potentially make voting
accessible to people who cannot vote today.

In email voting, voters fill out a ballot using standard software and
return it the county via email.

There is a lot to like about this system. It is much faster than paper
ballots, and it doesn’t require unfamiliar and expensive software.

Additionally, counting the votes can be automated, decreasing human
mistakes and costs.

Unfortunately, the system also has serious security flaws. When you
download a file or send an email, the data flows through many
intermediaries, who we neither know nor trust. The ballot can be
intercepted or modified on the way to the voter, and the vote can be
changed on the way back. Viruses on the voter’s computer can also
change the vote. It only takes one weak link.

To prove that this is not just a theoretical danger, we have
demonstrated that a normal home wireless router can be taken over and
made to intercept and modify ballots. The voter will send their ballot
as usual, but it is changed on its way. The change to the router is
undetectable even for security experts, and it only took two engineers
two days to develop.

Despite its benefits, voting by email is deeply flawed. As citizens,
we must demand a system where we can be confident that our votes are
counted.

Stay tuned for a demonstration.

## Daniel's version 1

Many governments are considering how to incorporate computers in their voting
systems, lured in part by the promise of cheap, accurate vote counting. One way
is to have voters fill out a ballot using standard software and return it to the
government by email.

There is a lot to like about this system. Voters can vote from the comfort of
their own home, and email is much faster than regular mail for returning the
ballot. Email software is familiar and inexpensive, and potentially makes voting
accessible to some people who cannot vote today. Counting votes can be
automated, decreasing costs and human mistakes.

Unfortunately, the system also has serious security flaws. When you download a
file or send an email, the data flows through many untrusted computer systems.
The ballot can be intercepted or modified on the way to the voter, viruses on
the voter's computer can change the vote, or the vote can be changed on its
return trip to the government. If any one of these attacks works, it can change
the outcome of an entire election.

To prove that this is not just a theoretical danger, we have demonstrated that a
normal home wireless router can be taken over from anywhere on the Internet.
Afterwards, the router will silently modify emailed votes after they leave the
voter's computer but before they arrive at the government. The demonstration
took only a few days to develop, and could easily be made very difficult to
detect, even for security experts.

Despite its benefits, voting by email is deeply flawed. We must demand a system
where we can be confident that our votes are counted. Stay tuned for a
demonstration.
