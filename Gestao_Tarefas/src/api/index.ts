import { Router } from 'express';
import task from './routes/taskRoute';

export default () => {
	const app = Router();

	task(app);

	return app
}
