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
    - ease of development: took two people two days [get the real data to put here] from inception to execution
    - failing mitigations
5. conclusion (5s)
    - serious, easily-exploitable problems with vote-by-email
    - now watch it in action
