import { Component } from '@angular/core';
import { MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'app-privacy-policy',
  templateUrl: './privacy-policy.component.html',
  styleUrls: ['./privacy-policy.component.css']
})
export class PrivacyPolicyComponent {
  constructor(private dialogRef: MatDialogRef<PrivacyPolicyComponent>) {}

  closeDialog(): void {
    this.dialogRef.close();
  }
}




