Example Demonstrators
=====================

Galois will be constructing demonstrators for *Overseas Vote
Foundation's End-to-end Verifiable Internet Election Project* (or *OVF
E2E VIV Project*) which we will be running for the next year.

One class of demonstrators are attacks on existing election
mechanisms. The other class is high-assurance prototypes of
frameworks, libraries, or working subsystems of E2E VIV election
systems.

Attacks on Existing Election Mechanisms
---------------------------------------

Election officials are using a variety of means by which to permit
overseas voters to participate in elections. Some of these mechanisms
are supported by the election research community; others are
not.

Those mechanisms that are not supported are often discouraged because
of risks associated with the security, auditability, correctness, or
legality of the mechanism in question.

Frequently experts highlight *potential* problems with these
mechanisms, but local election officials, states' legislatures, and
the U.S. Congress do not believe in the veracity of researchers claims
until they see a demonstrable attack.

Here are some examples of demonstrators of this class:

  * **[EMAIL]** Election officials are beginning to distribute blank
    ballots from an election website or via email; something that is
    supported by the elections research community. Unfortunately, some
    are also *receiving completed ballots* via email.

    More specifically, a voter downloads a PDF ballot, fills it out
    using Acrobat Reader or similar, then simply emails the resulting
    file as an attachment to a public email address.

    Election officials claim that such a receipt system is both secure
    and "not an internet voting system".

    This scenario mandates that we write an *email election
    subversion* demonstrator.

    Such a demonstrator can come in one of three forms:

    1. A root-kit or virus on the mail server which changes ballots
       stored in a mail server.
    2. A subverted email server which changes ballots as they arrive
       and before they are stored.
    3. A subverted router which changes ballots as they flow through
       the network.

  * **[MITM]** Internet election systems are frequently deployed with
    insecure network and server configurations. In particular, it is
    not uncommon to see certificate abuse (self-signed certificates,
    site certificates signed with unfamiliar CAs, and unpinned
    certificates) combined with no second-channel ballot certification
    mechanism (e.g., an SMS message to confirm your submitted ballot
    was stored properly).

    Performing a man-in-the-middle attack on a web-based internet
    voting system of this kind is straightforward, as one only need to
    do DNS spoofing to redirect voters to a replicated election
    website, perform an HTTP-based MITM so as to permit voters to
    complete their ballots, but the MITM votes as its wishes.

  * **[E2E]** End-to-end systems are meant to provide the *strong
    software independence* guarantee: any problem with the system's
    implementation, whether a bug or a hack, will be detected during
    or after the election. Unfortunately, rigorous engineering is
    never used to construct the E2E election system, thus the
    cryptographic algorithm which, on paper, should have this
    guarantee, does not in practice.

    Previous audits of the Helios voting system (the premier E2E
    system today) have revealed implementation errors that permitted
    election malfeasance. A full-election demonstrable attack that
    leverages one of these errors is a compelling demonstration of the
    need for high-assurance engineering even for E2E election systems.

    Such an attack would be crafted by (a) finding a flaw in the
    client

  * **[CRYPTO]** Client-side crypto is often used in internet election
    systems to authenticate voters and encrypt ballots for
    submission. Javascript is most often used as the client-side
    programming language; Java is occasionally used.

    A variety of attacks on such systems are possible given the lack
    of any sanity or sandboxing of Javascript. Moreover, all
    Javascript crypto implementations are bespoke and erroneous.

    One attack of interest is accomplished by injecting malicious
    Javascript into the voter's browser to manipulate what the voter
    sees, and how the voter votes, when using the system. For example,
    one might perform DOM manipulations to toggle candidates, thus you
    think you are voting for Obama but in fact you are voting for
    Romney. Another example is to manipulate the crypto so that it
    improperly encrypts ballots thereby violating voter privacy
    (especially when combined with an HTTP-aware MITM).

High-assurance Prototypes
-------------------------

Another class of demonstrators are high-assurance prototypes of
frameworks, libraries, or working subsystems of E2E VIV election
systems. Here are some example systems that we might choose to build.

  * **[USABILITY]** Measuring and reflecting upon the usability of E2E
    VIV election systems is challenging because the tight relationship
    between *user interaction* and the *security design* of any
    working system. Brainstorming on new user interfaces is easy, but
    measuring the usability of demonstrable interfaces is difficult.

    This difficulty is, in part, due to the expensive nature of
    performing usability studies: finding a large enough participant
    pool that represents the typical set of voters, the time it takes
    to organize and conduct the usability experiment itself, analyzing
    the results of the experiment and feeding those results in a
    sensible way back into the design process.

    But in the setting of E2E VIV election systems, this problem is
    compounded by the high cost of developing new systems on which to
    perform the experiments in the first place.

    Galois proposes that, while there are two standard ways in which
    we can build demonstrators to address these costs, only one of
    them is really useful in the context of E2E VIV election systems.

    1. System demonstrators can be constructed that deliberately
       perform "*security theater*"---that is to say, the systems
       behave as if their are implementing the underlying security
       mechanisms and fulfilling the system architecture (e.g.,
       running on a set of secure servers), when in fact the system
       executes only the conceptual work-flow of the election with no
       back-end security. The serious challenge with this kind of
       demonstrator is that the subtleties of E2E VIV cryptography
       typically "leak" out in to the UI. It is a non-trivial design
       and engineering challenge to circumvent these subtleties.

    2. Another common approach is to push the "theater" even farther:
       use *UI mock-ups* so that no software is written at all. User
       interfaces are mocked-up using paper & pencil or design
       software like Photoshop or InDesign. Usability experiments then
       are manually facilitated by experts who drive the faux user
       interfaces.

    Both of these standard methods are fraught with difficulties in
    the context of E2E VIV election systems, thus some serious thought
    needs to be put into engineering any kind of UI demonstrator.

    Complementing the above thinking, Galois proposes that there is
    another way to facilitate large-scale experimentation with UI
    alternatives: *a "devops" style of experimentation leveraging
    social media*. Our idea is to offer demonstrators to the general
    public via social media outreach, such as updates on Twitter,
    Facebook, and similar. This experimentation methodology permits us
    to concurrently run several experiments of different UIs over
    random online populations, much like Facebook tests new UI
    features. We believe that, with sufficient care put into the
    logging of user behavior, runtime analysis of user interactions
    should be able to inform the expert teams considerably about
    UI design decisions.

  * **[LOGGING]** Election logging is a critical subsystem in modern
    auditable elections. Unfortunately, no reusable trustworthy
    election logging system has ever been built, despite the amount of
    research work that has gone into defining what a forensic election
    logging system should look like.

    A prototype system for such already exists and is of interest to
    NIST and the U.S. Election Assistance Commission (EAC). It is
    written in JML-annotated Java has a BON system specification. It
    uses a runtime verification logic to specify trace
    properties. https://github.com/demtech/logging

  * **[POLLBOOKS]** Digital pollbooks are frequently used in elections
    in the U.S.A. and abroad. Pollbooks, also known as Digital Voter
    List systems, print ballots, manage candidate lists, manage voter
    lists, summarize election participation results, etc.

    A prototype system that supports list-based elections in Denmark
    is already built. It is written in C# and has a BON system
    specification and a Code Contracts-based low-level
    specification. Generalizing it for U.S. elections is of interest
    to the EAC. https://github.com/demtech/dvl

  * **[V4]** Building a complete prototype V4 system is very
    interesting to election officials and the EAC. This work needs
    expertise in writing and verifying C and Java code, 3D printing,
    and applied cryptography. https://github.com/demtech/v4

  * **[TESTING]** Uilioch is a model finder-based election test
    generator. Election schemes are specified using a typed
    first-order logic and a election generation harness generates
    non-isomorphic election results. Applying this system to
    U.S. election schemes is a result that NIST is very interested in.

    I have all of the system's code in a private repository. It is
    written in JML-annotated Java and has a BON system
    specification. Expertise in Alloy is necessary. It is meant to be
    imported from SVN to Git here: https://github.com/demtech/uilioch
