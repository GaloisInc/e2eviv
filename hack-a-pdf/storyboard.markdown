# Outline

1. introduction (15s)
    - recently re-examining voting systems
    - lured by promise of low cost, high accuracy counting using computers
    - one proposal is vote-by-email: email a PDF to voter, he emails a PDF back
2. promised features (10s)
    - remote voting
    - accessibility
    - low administrative overhead
        - few poll workers/locations needed
        - low hardware/software cost compared to other evoting systems
    - machine counting
    - fast, reliable ballot delivery and receipt
    - flexibility (e.g. personalized ballots)
3. possible vulnerabilities/expanded architecture diagram (30s)
    - email from government to voter passes along untrusted links
        - possible DOS attack
        - ballot could be copied or diverted to attacker to place replacement vote
        - forward on a modified PDF -- e.g. one with modified party descriptions, misidentified radio buttons, etc.
        - untrusted links include ones administered by experts, but also ones administered by non-experts
    - once on the user's computer, vote is cast on untrusted hardware with untrusted software
        - compromised PDF reader/writer could display/record vote incorrectly
        - compromised email program or operating system could modify votes
    - email from voter to government again passes along untrusted links
        - voter's choices visible to attacker
        - all same attacks as on the way in, but now more reliably attack people who are voting the "wrong" way
    - no real way to audit that these things didn't go wrong, either
4. our hack (20s)
    - description
        - voter sends in intended vote... but government receives something different!
        - contact home routers with default password
        - patch the software on the router to rewrite email attachments
        - looks for a specific vote, replace it with another specific vote
        - essentially invisible: patched software is, to all outward appearances, identical to the factory default software
    - ease of development: took two people two days [get the real data to put here] from inception to execution
    - failing mitigations
        - things that require more technical expertise/coordination than can be expected of the general public
            - end-to-end encrypted email connections
            - use digital signatures on the votes
            - harden all routers... and all hardware on the Internet
        - use encrypted PDFs (but still vulnerable to compromised software on voter's computer)
        - try to detect malfunctioning hardware (an unwinnable arms race)
5. conclusion (5s)
    - serious, easily-exploitable problems with vote-by-email
    - now watch it in action

# Transcript

[comment]: # (timings for each paragraph (in seconds): 17, 19, 24, 18, 23, 12)
[comment]: # (desired timings:                         15, 10,     30, 20,  5)
[comment]: # (total runtime 1:52)

Many governments have recently expressed interest in re-examining their voting
systems, lured in part by the promise of low-cost, high-accuracy automated vote
counting. One simple proposal has the government send each voter a PDF ballot
by email, which is filled out and emailed back to the government for counting.

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
