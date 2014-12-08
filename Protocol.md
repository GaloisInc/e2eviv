# STAR-Vote: Protocol Issues & Thoughts

This file exists to document potential issues with the protocol as
described in [the paper][paper]. Issues can include
under-specification, apparent inconsistencies, potential
incorrectness, and implementation concerns. It exists also to document
our solutions to these issues.

[paper]: https://www.usenix.org/system/files/conference/evtwote13/jets-0101-bell.pdf


## Provisional Ballots

The STAR-vote paper mentions provisional voting in only four
places. Additionally, it seems that two of them conflict. Here's a
description of the issue and a proposed solution.

Page 20: when describing the contents of the sticker printed out by
the check-in system (that contacts the voter status database), it
states:

> Provisional voters will be indicated with a suitable prefix to their
> precinct code, allowing the voting system to suitably distinguish
> their ballots from regular ones. (Provisional votes are cast by
> voters who, for whatever reason, do not appear in the voter
> registration database, and believe this to be in error. They are
> only tabulated after the voter’s registration status is verified,
> typically not until at least a few days after the end of voting.)

When describing the voting process as it is experience by users, it
states:

> In the case of a provisional ballot, the voter must return the
> printed ballot to a poll worker. The voter can choose to spoil the
> ballot and re-vote or to cast the ballot provisionally by having it
> placed—under an identifying seal—into a distinct provisional ballot
> box. The voter may retain the receipt to see if the ballot ends up
> being counted.  Because the ballot box is connected to the
> controller over the LAN, it can also query the controller as to
> whether the ballot is provisional. In the event that a voter
> accidentally puts a provisional ballot into the ballot box, the
> scanner can detect this and reject the printed ballot.  (Provisional
> ballots need to go into dedicated envelopes that are processed after
> the voting has ended.)

Later, after the analysis of the cryptography, it mentions absentee
and provisional voting together:

> 7.1.2. Absentee and provisional ballots.
>
> There are several methods available for incorporating ballots which
> are not cast within the STAR-Vote system, such as absentee and
> provisional ballots.  The simplest approach is to completely
> segregate votes and tallies, but this has several disadvantages,
> including a reduction in voter privacy and much lower assurance of
> the accuracy of the combined tally.
>
> It may be possible to eliminate all “external” votes by providing
> electronic means for capturing provisional and remote
> ballots. However, for the initial design of the STAR-Vote system, we
> have chosen to avoid this complexity. Instead, we ask that voting
> officials receive external votes and enter them into the STAR-Vote
> system as a proxy for voters. While this still does not allow remote
> voters to audit their own ballots, the privacy-preserving
> risk-limiting audit step is still able to detect any substantive
> deviations between the paper records of external voters and their
> electronically recorded ballots. This provides more supporting
> evidence of the veracity of the outcome without reducing voter
> privacy.

Finally, in the conclusion, it states:

> STAR-Vote also opens the door to a variety of interesting future
> directions. For example, while STAR-Vote is intended to service any
> given county as an island unto itself, there’s no reason why it
> cannot also support remote voting, where ballot definitions could be
> transmitted to a remote supervised kiosk, which securely returns the
> electronic and paper records. By virtue of STAR-Vote’s cryptographic
> mechanisms, such a remote vote is really no different than a local
> provisional vote and can be resolved in a similar fashion,
> preserving the anonymity of the voter. (A variation on this idea was
> earlier proposed as the RemoteBox extension (Sandler and Wallach
> 2008) to VoteBox (Sandler et al. 2008).) This could have important
> ramifications for overseas and military voters with access to a
> suitable impromptu polling place, e.g., on a military base or in a
> consular office.

Two separate means of indicating provisional votes are thus provided:

 1. Provisional voters use the same mechanism as everyone else to
    vote, with the exception that their ballots are selectively
    included in the count based on a later determination of
    eligibility.

 2. Provisional voters, as with absentee voters, are using the voting
    terminals as glorified pencils to fill out paper ballots, which
    are later entered by officials upon determination of eligibility.

If provisional voters are treated in the same manner as absentee
ballots, then provisional voters will not have receipts that they can
use to check if their votes are counted, conflicting with one of the
above statements. Even if the machine produces a receipt, the vote
will be hand-entered by an official at a later time. Because ElGamal
encryption relies on a random number, the hand-entered vote will have
a different receipt.

If provisional voters are going to be able to audit their votes, then
the protocol will require modification to support them. At the present
time, a ballot can have three states: "unknown", "cast", and
"spoiled". Only the "cast" votes are included in the final count. If
provisional ballots should be a part of the hash chain and be
auditable, the protocol could perhaps be extended with a fourth state:
"cast provisionally".

Remember that it is possible to identify provisional ballots: the main
ballot box must reject them, and they must be held in a separate
box. The act of putting a ballot into the provisional ballot box could
then place it in the "cast provisionally" state. After the election,
each provisional ballot that is approved can then be placed in the
"cast" state by election officials. This preserves the auditing
properties and prevents attacks on the anonymity of provisional votes
by mixing them with the non-provisional votes prior to counting. The
provisional status can still be identified from the paper ballot.


## Timeout for voting codes

Upon check-in with the controller/judge's station, voters receive a
five-digit code. This code is used to initiate the session with the
voting terminal, arranging for the correct ballot configuration to be
shown. The paper states:

> There are only ever a small number of 5-digit codes active at any
> one time, reducing the odds of a voter successfully guessing an
> active code and casting multiple ballots. We note that there will be
> no record binding the 5-digit code to the voter, helping ensure
> voter anonymity.

However, there is no specific mechanism proposed to expire the
codes. Any such mechanism must deal gracefully with the following
situations:

0. A voter checks in, their sticker is printed, but they never check
   in with the controller/judge's station.

1. A voter enters their code, but decides not to vote after all.

2. A voter enters their code, votes, and casts the ballot.

3. A voter enters their code, votes, and spoils one or more ballots
   before finally casting a ballot and leaving.

4. A voter with a disability not appropriately anticipated by the UI
   team takes three hours to fill out their ballot, then casts it,
   potentially spoiling one first either due to a mistake or to audit
   the terminal.

5. An eligible voter who is not interested in any candidates but
   nevertheless cares deeply about election integrity spoils a ballot
   from each terminal, but doesn't cast a ballot.

It is certainly not sufficient to allow codes to work for the
remainder of the election. It is reasonable to expect 15000 voters at
a single polling place. This means that we 15% of the potential codes
will be issued. Because we presumably will allow a typo, someone
voting at the end of the day could have a relatively large chance of
voting twice. Also, this would allow sneaky voters to take advantage
in a lapse in the poll workers' attention to vote again. We must
expire the codes. However, expiring them too early might deny some
voters, especially those who are poorly served by the interface on the
terminals, the ability to vote.

A code can be safely expired when a ballot is cast, as the voter has
no further interactions with the air-gapped network. In other cases,
we propose a countdown timer that can be reset by the terminal or the
controller/judge's station. These systems will reset the timer on each
interaction with the voter. For instance, selecting an option for a
single contest on a ballot should reset the timer, as should spoiling
a ballot. Experimentation and consultation with user experience and
universal accessibility experts can determine the correct duration.



## Ballot IDs vs Ballot Casting IDs

Each ballot in STAR-vote has two separate unique identification
numbers: a ballot ID and a ballot casting ID. The ballot ID must be
non-sequential, while the ballot casting ID is allowed to be
sequential or predictable.

They are mentioned the following places:

p. 20, on the list of what's printed:

> 1) a paper ballot which includes a human-readable summary of the
> voter’s selections and a random (non-sequential) serial number

p. 23:
> Ballot summaries deposited in a ballot box have their serial numbers
> scanned and recorded. The electronically stored encrypted vote is not
> considered complete (and not included in the tally) unless and until
> its corresponding serial number has been recorded in the ballot box.
> 
> Any electronically stored encrypted ballots for which no
> corresponding serial number has been scanned and recorded are deemed
> spoiled. The published election record should include all spoiled
> ballots as well as all cast ballots, but for each spoiled ballot the
> published record should also include a verifiable decryption of the
> ballot’s contents. Voters should be able to easily look up digitally-
> signed records for any receipts they hold and verify their presence
> and, for spoiled receipts, the ballot contents.
>
> A voter who takes a completed paper ballot summary to a poll worker
> can request that the poll worker spoil the ballot and give the voter
> an opportunity to re-vote. The poll worker marks both the take-home
> receipt and the paper ballot summary as spoiled (including removing
> or marking the serial number so that it will not be recorded if
> subsequently placed in the ballot box) and returns the spoiled
> ballot summary to the voter.


p. 29:

> It could also happen that a ballot enters the ballot box but its
> serial number is not picked up, so the electronic vote data ends up
> in the “untallied but unspoiled” group. This should be detectable by
> a compliance audit (Benaloh et al. 2011; Lindeman and Stark 2012;
> Stark and Wagner 2012) as a mismatch between the number of recorded
> votes and the number of pieces of paper, providing an opportunity to
> resolve the problem before the audit begins.

p. 31:

> 1. For each ballot, the ballot marking device selects a random
>    ballot id sequence number bid. This bid is printed on the ballots
>    as a barcode. Furthermore, for each race r to which the voter
>    participates, an encryption of H(bid||r) is also computed and
>    appended to the encryption of the choices.
>
>...
>
> 4. Now, auditors can sample the paper ballots, read the bid printed on
>    them, recompute the value of H(bid||r) for all races present on the
>    paper ballot, and compare to the electronic record (as well as
>    check many other things, as prescribed for the risk-limiting
>    audit). The use of hashed bid’s has the important benefit of making
>    sure that someone who does not know a bid value cannot, by looking
>    at the electronic record, link the selections made for the
>    different races on a single ballot, which protects from pattern
>    voting attacks. There is no need for such a protection from someone
>    who can access the paper ballots, since that person can already
>    link all races just by looking at the paper.

Later on p.31, in the protocol description:

> 3. When a voter completes the ballot marking process selection to
>    produce a ballot v, the voting terminal performs the following
>    operations:
> 
>    a. It selects a unique and unpredictable ballot identifier bid, as
>       well as a unique (but possibly predictable) ballot casting
>       identifier bcid.
>
>    b. It computes an encryption c\_v = E\_K(v) of the vote, as well as a
>       NIZK proof p\_v that c_v is an encryption of a valid ballot. This
>       proof is written in such a way that it can be verified from
>       Ext(c\_v) only.
>
>    c. For each race r1,...,rn to which the voter takes part, it
>       computes an encryption c\_bid =
>       E\_K(bid||r1)||···||E\_K(bid||r\_n).

Note, however, that this is different from the earlier encrypted hash
approach. Email correspondence with the paper authors led to us
determining that this is a bug. The proper definition is

c\_bid = E\_K(H(bid||r1))||...||E\_K(H(bid||rn))

so that bid isn't revealed by decrypting a ballot.

>    d. It computes a public hash code
>       z^p\_i=H(bcid||Ext(c\_v)||p\_v||m||z^p\_{i-1}), where m is the
>       voting terminal unique identifier, as well as an internal hash
>       z^i\_i=H(bcid||c\_v||p\_v||c\_bid||m||z^i\_{i-1})
> 
>    e. It prints a paper ballot in two parts. The first contains v in a
>       human readable format as well as c\_bid and bcid in a robust
>       machine readable format (e.g., as barcodes). The second is a
>       voter take-home receipt that includes, the voting terminal
>       identifier m, the date and time, and the hash code z^p\_i (or a
>       truncation thereof), all in a human-readable format.
>
>    f. It transmits (bcid,c\_v,p\_v, c\_bid, m, z^p\_i, z^i\_i) to the
>       judge’s station.
>
> 4. When a ballot is cast, the ballot casting id bcid is scanned and
>    sent to the judge’s station. The judge’s station then marks the
>    associated ballot as cast and ready to be included in the tally. This
>    information is also broadcast and added in the two hash chains

There seems to be some confusion as to whether bid, bcid, or both
appear on ballots.

A reasonable interpretation is that both should be on ballots. bcid
will then be used to link paper ballots to their electronic records
(cast/spoil decision), while the bid is used during the audit to check
that the encrypted hashes of the votes for some particular paper
ballot are included in the count. Because the bid exists *only* on the
paper ballot under this interpretation, it can only be used at fairly
large expense by someone with access to the paper ballots to check for
the presence of votes in the count.

