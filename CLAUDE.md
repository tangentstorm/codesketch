# CLAUDE.md - Assistance Guide

## Build & Test Commands
- Build: `npm run build` or `yarn build` 
- Lint: `npm run lint` or `yarn lint`
- Test (all): `npm test` or `yarn test`
- Test (single): `npm test -- -t "test name"` or `jest path/to/test.js`
- Dev server: `npm run dev` or `yarn dev`

## Code Style Guidelines
- **Formatting**: Use prettier for consistent formatting
- **Typing**: Use TypeScript with strict mode; avoid `any` types
- **Imports**: Group imports by external/internal; sort alphabetically
- **Naming**: camelCase for variables/functions, PascalCase for classes/components
- **Components**: One component per file, use functional components with hooks
- **Error Handling**: Use try/catch for async operations, provide meaningful error messages
- **Comments**: Document complex logic, use JSDoc for public APIs
- **Testing**: Write unit tests for logic, integration tests for workflows

Customize this file as your project evolves.