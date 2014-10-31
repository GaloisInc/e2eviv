Many governments have recently expressed interest in re-examining their voting
systems, lured in part by the promise of low-cost, high-accuracy automated vote
counting. One simple proposal has the government send each voter a PDF ballot
by email, which is filled out and emailed back to the government for counting.

![simplified system architecture](simple_architecture.jpg)

There is a lot to like about this system. It allows remote voting, and email is
a fast and familiar delivery mechanism for this. Because vote counting can be
automated, there is hope that this can be done without mistakes, and the system
can be deployed with low hardware, software, and election-day costs compared to
other evoting systems.

Unfortunately, the system also has serious security flaws. In a typical voting
lifecycle, a PDF flows through many pieces of untrusted hardware and software:
from the government's computers via Internet infrastructure to the voter's home
network, then through the voter's computer's operating system, email, and PDF
programs, and back again through the same process in reverse.

![more realistic system architecture](architecture.jpg)

If an attacker can compromise any part of this chain, they can influence the
outcome of the voting experience. Attacks include diverting a ballot from
its intended recipient so that the attacker may vote in their place or modifying
a ballot before or after the voter has cast their vote.

We have showed that the latter attack can be easily realized. In our
proof-of-concept, we remotely patch a router to modify votes contained in
emails. From the voter's point of view, they send an email with their intended
vote, but the government receives an entirely different vote! This kind of
attack is not even hard: from inception to execution, the idea took two people
two days to develop.

There are simply too many untrusted, unverifiable, insecure links in the chain
of voting for it to hold its weight. As attractive as it is, voting by email in
this way is deeply flawed. Stay tuned to see a demonstration.
