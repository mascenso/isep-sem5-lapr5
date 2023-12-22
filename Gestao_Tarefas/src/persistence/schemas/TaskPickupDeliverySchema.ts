import { ITaskPickupDeliveryPersistence } from '../../dataschema/ITaskPickupDeliveryPersistence';
import mongoose from 'mongoose';

const TaskPickupDeliverySchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    description: { type: String, unique: false },
    pickupLocalization: { type: Object, unique: false },
    deliveryLocalization:{ type: Object, unique: false },
    contactNumber:{ type: Number, unique: false },
    user:{ type: Object, unique: false },
    deliveryContact:{ type: Object, unique: false },
    pickupContact:{ type: Object, unique: false },
    approved: {type:Boolean,unique:false},
    pending: {type:Boolean,unique:false}
  },
  {
    timestamps: true
  }
);

export default mongoose.model<ITaskPickupDeliveryPersistence & mongoose.Document>('TaskPickupDelivery', TaskPickupDeliverySchema);
