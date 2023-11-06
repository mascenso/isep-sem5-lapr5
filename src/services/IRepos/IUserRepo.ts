import { Repo } from "../../core/infra/Repo";
import { User } from "../../domain/user-agg/user";
import { UserEmail } from "../../domain/user-agg/userEmail";

export default interface IUserRepo extends Repo<User> {
	save(user: User): Promise<User>;
	findByEmail (email: UserEmail | string): Promise<User>;
	findById (id: string): Promise<User>;
}
